import re
import markdown
import pandas as pd
from docx import Document
from bs4 import BeautifulSoup
from pathlib import Path


def df2dict(df, index = "id"):
    """
    Convert a DataFrame to a nested dictionary.

    This function takes a DataFrame and converts it into a nested dictionary,
    where each row of the DataFrame becomes a dictionary entry with the index
    column as the key. Missing values in the DataFrame are replaced with "None".

    Args:
        df (pandas.DataFrame): The DataFrame to convert.

    Returns:
        dict: A nested dictionary representation of the DataFrame, where each key
              is the index column value and each value is a dictionary representing
              the corresponding row of the DataFrame.
    """
    outcome = (
        df.copy()
        .fillna("None")
        .set_index(index)
        .to_dict(orient="index")
    )
    return outcome

def get_section_data(section_name, outline):
    """
    Get data for a specific section from an outline DataFrame.

    This function takes a section name and an outline DataFrame as input,
    and returns a dictionary containing data for the specified section.

    Args:
        section_name (str): The name of the section to retrieve data for.
        outline (pandas.DataFrame): The outline DataFrame containing section data.

    Returns:
        dict: A dictionary containing data for the specified section, with the following keys:
              - 'id': The ID of the section page.
              - 'header': The header of the section.
              - 'toc': A DataFrame containing the table of contents (TOC) for the section.
    """

    content = outline.loc[outline["section_header"] == section_name]
    data = {
        "id"      : content.loc[content["section_page"] == True, "id"].iloc[0],
        "header"  : content.loc[content["section_page"] == True, "section_header"].iloc[0],
        "toc"     : content.loc[content["has_subsection"] == True]
    }
    return data

def page_grouping(input):
    """
    Groups elements from the input list into sublists, where each sublist represents a page.

    Args:
        input (list of str): A list of strings, where each string is an element that can be part of a page. 
                             The special string "page-end" indicates the end of a page.

    Returns:
        list of list of str: A list of sublists, where each sublist contains elements of a page. The "page-end" 
                             strings are not included in the sublists.
    """
    page_group = []
    current    = []

    for element in input:
        if "page-end" in element:
            if current:
                page_group.append(current)
                current = []
        else:
            current.append(element)
    
    if current:
        page_group.append(page_group)

    return page_group

def get_page_data(page_id, outline, figure_map):
    """
    Retrieves and organizes data for a specific thematic page, including its sections, charts, and chart panels, from the given 
    outline and figure map.

    Args:
        page_id (str): The unique identifier of the page to retrieve data for.
        outline (pandas.DataFrame): A DataFrame containing metadata and structure information about the pages.
        figure_map (pandas.DataFrame): A DataFrame containing metadata and details about charts and their panels.

    Returns:
        dict: A nested dictionary containing the organized data for the specified page, structured into three levels:
            - Level 1: Basic page information including ID, even/odd page status, subsection headers, and charts.
            - Level 2: Details about each chart on the page, including ID, title, subtitle, footnotes, notes, and legend information.
            - Level 3: Detailed information about each panel within each chart, including panel titles, subtitles, and legend information.
    """

    # Level 1
    level1_targets = ["id", "page", "evenPage", "section_header", "subsection_header", "charts", "has_subsection", "legend"]
    level2_targets = ["id", "chart_title", "chart_subtitle", "chart_insight", "footnote", "source", "legend_text", "legend_color"]
    level3_targets = ["panel", "panel_title", "panel_subtitle", "legend_text", "legend_color"]
    level1_data    = df2dict(outline.loc[outline["id"] == page_id, level1_targets])
    level1_data    = level1_data[page_id]
    level1_data["id"] = page_id
    level1_data["section_header"] = re.sub(
        "Section.+: ", "", level1_data["section_header"]
    )

    # Level 2
    covered_figures = level1_data["charts"].split(", ")
    level2_data = df2dict(
        figure_map.loc[figure_map["id"].isin(covered_figures), level2_targets].drop_duplicates(subset = ["chart_title"])
    )
    for chart, data in level2_data.items():
        if "None" not in data["legend_text"]:
            data.update(
                {
                    "legend": dict(zip(
                        data["legend_text"].split(","),
                        data["legend_color"].split(",")
                    ))
                }
            )
        else:
            data.update(
                {"legend": "None"}
            )
    level1_data["charts"] = level2_data 

    # Level 3
    for chart in level1_data["charts"].keys():
        panel_data = (
            figure_map.loc[figure_map["id"] == chart, level3_targets]
            .assign(
                imgPath = lambda df: df.apply(lambda row: f"static/charts_and_images/{chart}/{chart}_{row['panel']}.svg", axis=1)
            )
        )
        level3_data = df2dict(panel_data, index = "panel")
        for panel, data in level3_data.items():
            if "None" not in data["legend_text"]:
                data.update(
                    {
                        "legend": dict(zip(
                            data["legend_text"].split(","),
                            data["legend_color"].split(",")
                        ))
                    }
                )
            else:
                data.update(
                    {"legend": "None"}
                )
        level1_data["charts"][chart]["ChartNo"] = chart.replace("Figure_", "Chart ").replace("_", ".")
        level1_data["charts"][chart]["panels"]  = level3_data

    return level1_data

def load_markdown_file(file_path, box = False):
    """
    Load a markdown file and wrap its paragraphs in HTML <p> tags. 

    This function reads the content of a markdown file from the specified file path,
    splits the content into paragraphs by double newlines, and wraps each paragraph
    in HTML <p> tags.

    Args:
        file_path (str): The path to the markdown file.

    Returns:
        str: A string containing the content of the markdown file with paragraphs
             wrapped in HTML <p> tags.
    """
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read()
    
    paragraphs = content.split('\n\n')
    wrapped_paragraphs = [f'<p>{paragraph.strip()}</p>' for paragraph in paragraphs]
    result = '\n\n'.join(wrapped_paragraphs)

    if box:
        result = re.sub(
            "<p>{% BOX %}",
            '</div><div class="col-12 mb-4 bg-quote p-5"><p class="p-quote"><i>',
            result
        )
        result = re.sub(
            "{% BOX END %}</p>",
            '</i></p></div><div class="col-12">',
            result
        )

    return result

def process_about_report_with_footnotes(file_path):
    """
    Process about_this_report.md with footnote support.
    
    Converts ^1, ^2, etc. to superscript links and separates footnotes that start with ^
    at the beginning of a line.
    
    Returns:
        dict: {'content': main text HTML, 'footnotes': list of footnote HTML strings}
    """
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read()
    
    # Split into paragraphs
    paragraphs = content.split('\n\n')
    
    main_content_paragraphs = []
    footnotes = []
    
    for paragraph in paragraphs:
        paragraph = paragraph.strip()
        
        # Check if this is a footnote (starts with ^ at beginning of line)
        if paragraph.startswith('^') and not paragraph.startswith('^{'):
            # This is a footnote definition
            # Extract the text after ^
            footnote_text = paragraph[1:].strip()
            # Convert URLs to clickable links in footnotes (excluding trailing punctuation)
            footnote_text = re.sub(
                r'(https?://[^\s<]+?)([.,;:!?]*)(\s|$)',
                r'<a href="\1" target="_blank">\1</a>\2\3',
                footnote_text
            )
            footnotes.append(footnote_text)
        else:
            # Process inline footnote references (^1, ^2, etc.)
            # Convert ^1 to <sup>1</sup>
            paragraph = re.sub(r'\^(\d+)', r'<sup>\1</sup>', paragraph)
            # Add to main content
            main_content_paragraphs.append(paragraph)
    
    # Join paragraphs and wrap in <p> tags (similar to load_markdown_file)
    wrapped_paragraphs = [f'<p>{p}</p>' for p in main_content_paragraphs]
    result = '\n\n'.join(wrapped_paragraphs)
    
    # Now handle BOX markers (like in load_markdown_file)
    result = re.sub(
        "<p>{% BOX %}",
        '</div><div class="col-12 mb-4 bg-quote p-5"><p class="p-quote"><i>',
        result
    )
    result = re.sub(
        "{% BOX END %}</p>",
        '</i></p></div><div class="col-12">',
        result
    )
    
    return {
        'content': result,
        'footnotes': footnotes
    }

def process_word(path):
    """
    Processes a Word document, extracting and formatting the text content with HTML-like tags for bold and italic text.

    Args:
        path (str): The file path to the Word document.

    Returns:
        list of str: A list of strings, where each string represents a paragraph from the document with text wrapped 
                     in HTML-like <b> and <i> tags to indicate bold and italic formatting, respectively.
    """
    doc = Document(path)
    content = []
    
    for paragraph in doc.paragraphs:
        paragraph_text = ""
        run_buffer = ""
        bold = italic = False

        for run in paragraph.runs:
            run_text = run.text

            if run.bold != bold or run.italic != italic:
                if run_buffer:
                    if bold:
                        run_buffer = f'<b>{run_buffer}</b>'
                    if italic:
                        run_buffer = f'<i>{run_buffer}</i>'
                    paragraph_text += run_buffer
                    run_buffer = ""

                bold = run.bold
                italic = run.italic

            run_buffer += run_text

        if run_buffer:
            if bold:
                run_buffer = f"<b>{run_buffer}</b>"
            if italic:
                run_buffer = f"<i>{run_buffer}</i>"
            paragraph_text += run_buffer

        content.append(paragraph_text)
    
    grouped_content = page_grouping(content)

    return grouped_content

def get_dynamic_data(general_info, outline,  methodological_materials_df):
    """
    Get dynamic data for generating a report.

    This function takes an outline DataFrame as input and returns a dictionary
    containing dynamic data for generating a report, including general information,
    table of contents, and acknowledgements.

    Args:
        outline (pandas.DataFrame): The outline DataFrame containing report structure data.

    Returns:
        dict: A dictionary containing dynamic data for generating a report, with the following keys:
              - 'general': General information about the report, including title, subtitle, 
                           description, and PDF name.
              - 'toc': A nested dictionary representing the table of contents (TOC) of the report.
              - 'acknowledgements': Acknowledgements text loaded from a markdown file.
    """

    materials = methodological_materials_df.to_dict(orient='records')
    methodological_materials = {
        "name"        : [source["material_name"] for source in materials],
        "description" : [source["description"] for source in materials],
        "link1"       : [source.get("link1", "") if pd.notna(source.get("link1")) and str(source.get("link1")).lower() != "none" else "" for source in materials],
        "label1"      : [source.get("label1", "") if pd.notna(source.get("label1")) and str(source.get("label1")).lower() != "none" else "" for source in materials],
        "link2"       : [source.get("link2", "") if pd.notna(source.get("link2")) and str(source.get("link2")).lower() != "none" else "" for source in materials],
        "label2"      : [source.get("label2", "") if pd.notna(source.get("label2")) and str(source.get("label2")).lower() != "none" else "" for source in materials],
        "link3"       : [source.get("link3", "") if pd.notna(source.get("link3")) and str(source.get("link3")).lower() != "none" else "" for source in materials],
        "label3"      : [source.get("label3", "") if pd.notna(source.get("label3")) and str(source.get("label3")).lower() != "none" else "" for source in materials],
        "header"      : outline.loc[outline["id"] == "Materials", "section_header"].iloc[0],
        "evenPage"    : outline.loc[outline["id"] == "Materials", "evenPage"].iloc[0],
        "page"        : outline.loc[outline["id"] == "Materials", "page"].iloc[0]
    }
    dynamic_data = {
        "general": (
            general_info
            .set_index('id')
            .T
            .to_dict(orient = "index")
        )["value"],
        "toc" : (
                df2dict(
                    outline
                    .loc[outline["toc"] == True, ["id", "page", "evenPage", "section_header", "subsection_header"]]
                )
        ),
        "acknowledgements" : {
            "text"     : markdown.markdown(load_markdown_file("text/acknowledgements.md")),
            "evenPage" : outline.loc[outline["id"] == "Acknowledgements", "evenPage"].iloc[0]
        },
        "aboutReport" : {
            "section_page" : get_section_data("About this Report", outline),
            **process_about_report_with_footnotes("text/about_this_report.md"),
            "text"         : markdown.markdown(process_about_report_with_footnotes("text/about_this_report.md")['content']),
            "page"         : outline.loc[outline["subsection_header"] == "About this Report", "page"].iloc[0],
            "header"       : outline.loc[outline["subsection_header"] == "About this Report", "section_header"].iloc[0],
            "evenPage"     : outline.loc[outline["subsection_header"] == "About this Report", "evenPage"].iloc[0]
        },
        "ExecFindings" : df2dict(
            outline.copy()
            .loc[outline["subsection_header"] == "Executive Findings", ["id", "page", "subsection_header", "evenPage"]]
            .assign(
                content = lambda df: [item['content'] for item in _distribute_exec_findings_content("text/executive_findings.md", len(df))],
                footnotes = lambda df: [item['footnotes'] for item in _distribute_exec_findings_content("text/executive_findings.md", len(df))],
                all_footnotes = lambda df: [item['all_footnotes'] for item in _distribute_exec_findings_content("text/executive_findings.md", len(df))],
                startingPage = lambda df: outline.loc[outline["subsection_header"] == "Executive Findings", "page"].iloc[0]
            )
        ),
        "Project"  : {
            "section_page" : get_section_data("Project Design", outline),
            "page"         : outline.loc[outline["section_header"] == "Project Design", "page"].iloc[0],
            "header"       : outline.loc[outline["section_header"] == "Project Design", "section_header"].iloc[0],
            "evenPage"     : outline.loc[outline["section_header"] == "Project Design", "evenPage"].iloc[0]
        },
         "Appendix"  : {
            "section_page" : get_section_data("Appendix", outline),
            "page"         : outline.loc[outline["section_header"] == "Appendix", "page"].iloc[0],
            "header"       : outline.loc[outline["section_header"] == "Appendix", "section_header"].iloc[0],
            "evenPage"     : outline.loc[outline["section_header"] == "Appendix", "evenPage"].iloc[0]
        },
        "AboutWJP"  : {
            "page"      : outline.loc[outline["id"] == "AboutWJP", "page"].iloc[0],
            "header"    : outline.loc[outline["id"] == "AboutWJP", "section_header"].iloc[0],
            "evenPage"  : outline.loc[outline["id"] == "AboutWJP", "evenPage"].iloc[0]
        },
        "BackCover" : {
            "page"      : outline.loc[outline["id"] == "BackCover", "page"].iloc[0]
        },

        "methodological_materials" : methodological_materials,
        "methodology" : {
            **process_methodology_markdown("text/methodology.md"),
            "evenPage"          : outline.loc[outline["id"] == "Methodology1", "evenPage"].iloc[0],
            "header"            : outline.loc[outline["id"] == "Methodology1", "section_header"].iloc[0],
            "id"                : outline.loc[outline["id"] == "Methodology1", "id"].iloc[0],
            "page"              : outline.loc[outline["id"] == "Methodology1", "page"].iloc[0],
            "subsection_header" : outline.loc[outline["id"] == "Methodology1", "subsection_header"].iloc[0],
        },
        "description_sample" : {
            "text"              : process_sample_description_markdown("text/sample_description.md"),
            "evenPage"          : outline.loc[outline["id"] == "Methodology2", "evenPage"].iloc[0],
            "header"            : outline.loc[outline["id"] == "Methodology2", "section_header"].iloc[0],
            "id"                : outline.loc[outline["id"] == "Methodology2", "id"].iloc[0],
            "page"              : outline.loc[outline["id"] == "Methodology2", "page"].iloc[0],
            "subsection_header" : outline.loc[outline["id"] == "Methodology2", "subsection_header"].iloc[0],
        }
        }
    
    return dynamic_data

def get_thematic_parameters(id, outline, figure_map):
    macro = outline.loc[outline["id"] == id].macro.iloc[0]

    if macro == "section":
        header     = outline.loc[outline["id"] == id].section_header.iloc[0]
        parameters = get_section_data(header, outline)
    
    if macro in ["singlepanel", "bipanel", "tripanel", "quadpanel", "pentapanel", "hexpanel"]:
        parameters = get_page_data(id, outline, figure_map)

    return parameters


import re

def process_methodology_markdown(file_path):
    """
    Process Methodology markdown file with footnote support.
    
    Returns a dict with:
    - 'paragraphs': list of HTML strings
    - 'footnotes': dict mapping footnote numbers to text {num: text}
    """
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read()
    
    paragraphs = []
    footnotes = {}  # Changed to dict to preserve numbers
    lines = content.split('\n')
    
    footnote_pattern = re.compile(r'^\^(\d+):(.*)$')
    
    for line in lines:
        line = line.strip()
        
        if not line:  # Skip empty lines
            continue
        
        # Check if this is a footnote definition (^N:text)
        footnote_match = footnote_pattern.match(line)
        if footnote_match:
            footnote_num = footnote_match.group(1)
            footnote_text = footnote_match.group(2).strip()
            # Convert URLs to links in footnote text
            footnote_text = re.sub(
                r'(https?://[^\s<]+?)([.,;:!?]*)(\s|$)',
                r'<a href="\1" target="_blank">\1</a>\2\3',
                footnote_text
            )
            footnotes[footnote_num] = footnote_text
            continue
        elif line.startswith('^') and not line.startswith('^{') and ':' not in line:
            # Footnote without number (^text) - auto-number sequentially
            footnote_text = line[1:].strip()
            footnote_text = re.sub(
                r'(https?://[^\s<]+?)([.,;:!?]*)(\s|$)',
                r'<a href="\1" target="_blank">\1</a>\2\3',
                footnote_text
            )
            # Auto-assign next available number
            next_num = str(len(footnotes) + 1)
            footnotes[next_num] = footnote_text
            continue
        
        # Replace markdown bold and italic with HTML
        line = re.sub(r'\*\*(.*?)\*\*', r'<b>\1</b>', line)
        line = re.sub(r'\*(.*?)\*', r'<i>\1</i>', line)
        
        # Convert ^N references to superscript
        line = re.sub(r'\^(\d+)', r'<sup>\1</sup>', line)
        
        # Handle page breaks
        if '<!-- page-end -->' in line:
            paragraphs.append('<!-- page-end -->')
        # Handle headers - preserve the markdown format for template to recognize
        elif line.startswith('#####'):
            paragraphs.append(line)
        elif line.startswith('####'):
            paragraphs.append(line)
        elif line.startswith('###'):
            paragraphs.append(line)
        elif line.startswith('##'):
            # Convert ## to bold HTML with same spacing as h1 (#)
            content_text = line.replace('##', '').strip()
            paragraphs.append(f'<h2 class="mt-4 mb-3"><b>{content_text}</b></h2>')
        elif line.startswith('#'):
            paragraphs.append(line)
        else:
            # Regular paragraph
            paragraphs.append(line)
    
    return {'paragraphs': paragraphs, 'footnotes': footnotes}


def process_sample_description_markdown(file_path):
    """
    Process Sample Description markdown file into structured sections.
    
    Returns a list of dictionaries with {title: ..., content: ...} structure
    for the DS (Description Sample) template.
    """
    with open(file_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()

    sections = []
    current_section = None

    for line in lines:
        if line.startswith('## '):
            if current_section:
                sections.append(current_section)
            current_section = {
                'title': line.strip().replace('## ', ''),
                'content': ''
            }
        elif current_section:
            # Replace markdown bold with HTML bold tags
            line = re.sub(r'\*\*(.*?)\*\*', r'<b>\1</b>', line)
            # Replace markdown italic with HTML italic tags
            line = re.sub(r'\*(.*?)\*', r'<i>\1</i>', line)
            current_section['content'] += line

    if current_section:
        sections.append(current_section)

    return sections


def process_markdown_for_exec_findings(file_path):
    """
    Process Executive Findings markdown file with footnote support.
    
    Returns a dict with:
    - 'paragraphs': list of HTML strings
    - 'footnotes': dict mapping footnote numbers to text
    """
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read()
    
    paragraphs = []
    footnotes = {}  # {footnote_number: footnote_text}
    lines = content.split('\n')
    
    footnote_pattern = re.compile(r'^\^(\d+):(.*)$')
    
    for line in lines:
        original_line = line
        line = line.strip()
        
        if not line:  # Skip empty lines
            continue
        
        # Check if this is a footnote definition (^N:text)
        footnote_match = footnote_pattern.match(line)
        if footnote_match:
            footnote_num = footnote_match.group(1)
            footnote_text = footnote_match.group(2).strip()
            
            # Convert URLs to links in footnote text
            footnote_text = re.sub(
                r'(https?://[^\s<]+?)([.,;:!?]*)(\s|$)',
                r'<a href="\1" target="_blank">\1</a>\2\3',
                footnote_text
            )
            
            footnotes[footnote_num] = footnote_text
            continue
        
        # Replace markdown bold and italic with HTML
        line = re.sub(r'\*\*(.*?)\*\*', r'<b>\1</b>', line)
        line = re.sub(r'\*(.*?)\*', r'<i>\1</i>', line)
        
        # Convert ^N references to superscript
        line = re.sub(r'\^(\d+)', r'<sup>\1</sup>', line)
        
        # Handle page breaks
        if '<!-- page-end -->' in line:
            paragraphs.append('<!-- page-end -->')
        # Handle headers - preserve the markdown format for template to recognize
        elif line.startswith('#####'):
            paragraphs.append(line)
        elif line.startswith('####'):
            paragraphs.append(line)
        elif line.startswith('###'):
            paragraphs.append(line)
        elif line.startswith('##'):
            paragraphs.append(line)
        elif line.startswith('#'):
            paragraphs.append(line)
        else:
            # Regular paragraph
            paragraphs.append(line)
    
    return {'paragraphs': paragraphs, 'footnotes': footnotes}


def _distribute_exec_findings_content(file_path, num_entries):
    """
    Distribute Executive Findings content across multiple entries based on page breaks.
    
    Splits content by <!-- page-end --> markers and distributes each page group
    to a separate entry. 
    
    For web version: All footnotes appear at the end of the section (last entry).
    For print version: Footnotes appear on each page where they are referenced (controlled by CSS).
    """
    processed_data = process_markdown_for_exec_findings(file_path)
    all_paragraphs = processed_data['paragraphs']
    all_footnotes = processed_data['footnotes']
    
    # Split by page breaks and track which footnotes are referenced on each page
    page_groups = []
    page_footnotes = []
    current_page = []
    current_page_footnotes = set()
    
    for para in all_paragraphs:
        if para == '<!-- page-end -->':
            if current_page:
                page_groups.append(current_page)
                # Build footnote dict for this page (for print version)
                page_footnote_dict = {num: all_footnotes[num] for num in current_page_footnotes if num in all_footnotes}
                page_footnotes.append(page_footnote_dict)
                current_page = []
                current_page_footnotes = set()
        else:
            current_page.append(para)
            # Find footnote references in this paragraph (e.g., <sup>2</sup>)
            import re
            footnote_refs = re.findall(r'<sup>(\d+)</sup>', para)
            current_page_footnotes.update(footnote_refs)
    
    # Add the last page if it has content
    if current_page:
        page_groups.append(current_page)
        page_footnote_dict = {num: all_footnotes[num] for num in current_page_footnotes if num in all_footnotes}
        page_footnotes.append(page_footnote_dict)
    
    # If no page breaks exist, treat all content as one page
    if not page_groups:
        page_groups = [all_paragraphs]
        # Find all footnote references in the content
        all_refs = set()
        for para in all_paragraphs:
            footnote_refs = re.findall(r'<sup>(\d+)</sup>', para)
            all_refs.update(footnote_refs)
        page_footnote_dict = {num: all_footnotes[num] for num in all_refs if num in all_footnotes}
        page_footnotes = [page_footnote_dict]
    
    # Distribute pages to entries
    result = []
    is_last_page_index = len(page_groups) - 1
    
    for i in range(num_entries):
        if i < len(page_groups):
            # Each page gets its specific footnotes (for print version)
            page_specific_footnotes = page_footnotes[i] if i < len(page_footnotes) else {}
            
            # Last page also gets ALL footnotes (for web version)
            is_last = (i == is_last_page_index)
            
            result.append({
                'content': page_groups[i],
                'footnotes': page_specific_footnotes,  # For print: page-specific
                'all_footnotes': all_footnotes if is_last else {}  # For web: all at end
            })
        else:
            result.append({'content': [], 'footnotes': {}, 'all_footnotes': {}})
    
    return result



