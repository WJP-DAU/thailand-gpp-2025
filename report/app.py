import pandas as pd
from flask import Flask, render_template
from bs4 import BeautifulSoup
from bs4.formatter import HTMLFormatter
from templates import functions

app = Flask(__name__)

general_info = pd.read_excel("../report_outline.xlsx", sheet_name = "general_info")
outline = (
    pd.read_excel("../report_outline.xlsx", sheet_name = "outline")
    .assign(
        evenPage = lambda x: x["page"] % 2 == 0
    )
)
figure_map   = pd.read_excel("../report_outline.xlsx", sheet_name = "figure_map")
methodological_materials_df = pd.read_excel("../report_outline.xlsx", sheet_name="methodological_materials")
other_publications_df = pd.read_excel("../report_outline.xlsx", sheet_name = "other_publications")
dynamic_data = functions.get_dynamic_data(general_info, outline, methodological_materials_df)
otherPublications = other_publications_df.to_dict(orient='records')

# Create the publications data
publications_data = {
    "publications": [
        {
            "onClick": pub["onClick"],
            "img": pub["img"],
            "href": pub["href"],
            "text": pub["text"]
        }
        for pub in otherPublications
    ],
    "header": outline.loc[outline["id"] == "Other Publications", "section_header"].iloc[0],
    "evenPage": outline.loc[outline["id"] == "Other Publications", "evenPage"].iloc[0],
    "page": outline.loc[outline["id"] == "Other Publications", "page"].iloc[0]
}

# Add to dynamic_data
dynamic_data["otherPublications"] = publications_data
thematic_findings = dict(
    zip(
        outline.loc[outline["thematic_findings"] == True].id, 
        outline.loc[outline["thematic_findings"] == True].macro
    )
)

@app.context_processor
def utility_processor():
    return dict(get_thematic_parameters = functions.get_thematic_parameters)

@app.route("/")
def report():

    rendered_html = render_template(
        "index.html", 
        dynamic_data      = dynamic_data,
        outline           = outline,
        figure_map        = figure_map,
        thematic_findings = thematic_findings 
    )
    soup = BeautifulSoup(rendered_html, 'html.parser')
    pretty_html = soup.prettify(formatter = HTMLFormatter(indent=4))
    with open("index.html", "w", encoding = "utf-8") as f:         
        f.write(pretty_html)   

    return pretty_html

if __name__ == "__main__":
    app.run(debug=True)
