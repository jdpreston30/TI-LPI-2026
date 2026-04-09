"""
Thoracic Irrigation Clinical Algorithm
=======================================
Scripted flowchart using graphviz (Python).

Requirements:
    pip3 install graphviz
    brew install graphviz   (macOS)

Run:
    python3 Python/thoracic_irrigation_algorithm.py

Output:
    Outputs/Figures/thoracic_irrigation_algorithm.pdf  (+ .png)
"""

import graphviz
import os

OUTPUT_DIR = os.path.join(os.path.dirname(__file__), "..", "Outputs", "Figures")
os.makedirs(OUTPUT_DIR, exist_ok=True)

# ---------------------------------------------------------------------------
# Shared style constants
# ---------------------------------------------------------------------------
FONT       = "Arial"
FONT_SIZE  = "13"
NODE_COLOR = "#ffffff"
EDGE_COLOR = "#333333"

STYLE_ACTION = dict(
    shape="box",
    style="rounded,filled",
    fillcolor=NODE_COLOR,
    color="#333333",
    penwidth="1.8",
    fontname=FONT,
    fontsize=FONT_SIZE,
    margin="0.18,0.12",
)
STYLE_DECISION = dict(
    shape="diamond",
    style="filled",
    fillcolor="#f0f4fa",
    color="#2c5f8a",
    penwidth="1.8",
    fontname=FONT,
    fontsize=FONT_SIZE,
    margin="0.10,0.10",
)
STYLE_START = dict(
    shape="oval",
    style="filled",
    fillcolor="#113d6a",
    fontcolor="white",
    color="#113d6a",
    penwidth="2",
    fontname=FONT,
    fontsize=FONT_SIZE,
)
STYLE_END_YES = dict(
    shape="box",
    style="rounded,filled",
    fillcolor="#c8dff5",
    color="#2c5f8a",
    penwidth="1.8",
    fontname=FONT,
    fontsize=FONT_SIZE,
    margin="0.18,0.12",
)
STYLE_STEP = dict(
    shape="box",
    style="rounded,filled",
    fillcolor="#ffffff",
    color="#333333",
    penwidth="1.6",
    fontname=FONT,
    fontsize=FONT_SIZE,
    margin="0.22,0.14",
    align="left",
)
STYLE_PREP = dict(
    shape="box",
    style="rounded,filled",
    fillcolor="#d6e4f7",
    color="#1a4f8a",
    penwidth="1.8",
    fontname=FONT,
    fontsize=FONT_SIZE,
    margin="0.22,0.14",
    align="left",
)
STYLE_LOOP = dict(
    shape="box",
    style="rounded,filled",
    fillcolor="#fff8e1",
    color="#b45309",
    penwidth="1.6",
    fontname=FONT,
    fontsize=FONT_SIZE,
    margin="0.22,0.14",
)
STYLE_RESOLVE = dict(
    shape="box",
    style="rounded,filled",
    fillcolor="#e0f2f1",
    color="#00695c",
    penwidth="1.6",
    fontname=FONT,
    fontsize=FONT_SIZE,
    margin="0.22,0.14",
)
STYLE_NEUTRAL = dict(
    shape="box",
    style="rounded,filled",
    fillcolor="#eeeeee",
    color="#757575",
    penwidth="1.6",
    fontname=FONT,
    fontsize=FONT_SIZE,
    margin="0.22,0.14",
)
STYLE_END_BOX = dict(
    shape="box",
    style="rounded,filled",
    fillcolor="#e8f5e9",
    color="#2e7d32",
    penwidth="1.8",
    fontname=FONT,
    fontsize=FONT_SIZE,
    margin="0.22,0.14",
)

# ---------------------------------------------------------------------------
# Build diagram
# ---------------------------------------------------------------------------
g = graphviz.Digraph(
    name="thoracic_irrigation_algorithm",
    graph_attr=dict(
        rankdir="TB",
        splines="ortho",
        nodesep="0.45",
        ranksep="0.35",
        fontname=FONT,
        bgcolor="white",
        size="8.5,22",
        dpi="400",
    ),
    edge_attr=dict(
        color=EDGE_COLOR,
        penwidth="1.5",
        fontname=FONT,
        fontsize="11",
    ),
)

# ── Indication ──────────────────────────────────────────────────────────────
g.node("start",
    "Patient with hemothorax on CT scan",
    **STYLE_START)

g.node("d_indication",
    "Hemothorax > 300 cc\nOR max effusion depth > 3 cm\nOR thoracostomy tube placement in OR?",
    **{**STYLE_DECISION, "fontsize": "11"})

g.node("observe",
    "Routine Monitoring /\nManagement",
    **STYLE_NEUTRAL)

# ── Step 1-2: Positioning & Tube ────────────────────────────────────────────
g.node("proceed",
    "Place thoracostomy tube and proceed\nwith thoracic irrigation",
    **STYLE_PREP)

# ── Steps 5-6: Irrigate & Evacuate (loop) ───────────────────────────────────
g.node("irrigate",
    "Irrigate with 500 cc WARM sterile normal saline",
    **STYLE_LOOP)

g.node("evacuate",
    "Evacuate fluid with suction (80–120 mmHg)",
    **STYLE_LOOP)

g.node("d_clear",
    "Effluent runs clear?",
    **STYLE_DECISION)

# ── Post-irrigation ──────────────────────────────────────────────────────────
g.node("connect",
    "Connect irrigator port to chest tube and collection device",
    **STYLE_RESOLVE)

g.node("cxr",
    "Repeat chest X-ray post-procedure",
    **STYLE_RESOLVE)

g.node("followup",
    "Follow retained hemothorax guideline\n"
    "for further management",
    **STYLE_END_BOX)

# ── Edges ────────────────────────────────────────────────────────────────────
# Label nodes: small filled circles sitting on decision output arrows
STYLE_LBL_NODE = dict(
    shape="circle",
    style="filled",
    fillcolor="#ffffff",
    color="#333333",
    penwidth="1.4",
    fontname=FONT,
    fontsize="13",
    fontcolor="#111111",
    width="0.53",
    height="0.53",
    fixedsize="true",
)
STYLE_LBL_NODE_WARN = dict(
    shape="circle",
    style="filled",
    fillcolor="#fff8e1",
    color="#b45309",
    penwidth="1.4",
    fontname=FONT,
    fontsize="13",
    fontcolor="#111111",
    width="0.53",
    height="0.53",
    fixedsize="true",
)

g.node("lbl_no1",  "<<B>NO</B>>",  **STYLE_LBL_NODE)
g.node("lbl_yes1", "<<B>YES</B>>", **STYLE_LBL_NODE)

lbl_yes2_style = {**STYLE_LBL_NODE, "width": "0.90", "height": "0.90"}
lbl_no2_style  = {**STYLE_LBL_NODE_WARN, "width": "0.90", "height": "0.90"}
g.node("lbl_yes2", "<<B>YES</B><BR/>(~1\u20132 L)>", **lbl_yes2_style)
g.node("lbl_no2",  "<<B>NO</B><BR/>repeat>",         **lbl_no2_style)

# Plain edges (no labels)
EDGE_PLAIN  = dict(arrowhead="normal")
EDGE_NOARR  = dict(arrowhead="none")

g.edge("start",        "d_indication", **EDGE_PLAIN)

# d_indication: same rank → observe LEFT, d_indication CENTER, proceed RIGHT
# lbl_no1 sits between observe and d_indication (same rank)
# lbl_yes1 sits between d_indication and proceed (same rank)
with g.subgraph() as s:
    s.attr(rank="same")
    s.node("observe")
    s.node("lbl_no1")
    s.node("d_indication")
    s.node("lbl_yes1")
    s.node("proceed")

# Enforce left-to-right ordering with invisible spacers
g.edge("observe",     "lbl_no1",    style="invis")
g.edge("lbl_no1",     "d_indication", style="invis")
g.edge("d_indication","lbl_yes1",   style="invis")
g.edge("lbl_yes1",    "proceed",    style="invis")

g.edge("d_indication", "lbl_no1",  **EDGE_NOARR)
g.edge("lbl_no1",      "observe",  **EDGE_PLAIN)
g.edge("d_indication", "lbl_yes1", **EDGE_NOARR)
g.edge("lbl_yes1",     "proceed",  **EDGE_PLAIN)

g.edge("proceed",      "irrigate", **EDGE_PLAIN)
g.edge("irrigate",     "evacuate", **EDGE_PLAIN)
g.edge("evacuate",     "d_clear",  **EDGE_PLAIN)

# d_clear → Yes → connect
g.edge("d_clear",      "lbl_yes2", **EDGE_NOARR)
g.edge("lbl_yes2",     "connect",  **EDGE_PLAIN)

# d_clear → No → right out to lbl_no2, then ortho routes straight up then left to irrigate
with g.subgraph() as s:
    s.attr(rank="same")
    s.node("d_clear")
    s.node("lbl_no2")
g.edge("d_clear",  "lbl_no2",  tailport="e", headport="w", **EDGE_NOARR)
g.edge("lbl_no2",  "irrigate", arrowhead="normal", constraint="false", tailport="n", headport="e")

g.edge("connect",      "cxr",      **EDGE_PLAIN)
g.edge("cxr",          "followup", **EDGE_PLAIN)

# ---------------------------------------------------------------------------
# Render
# ---------------------------------------------------------------------------
out_base = os.path.join(OUTPUT_DIR, "thoracic_irrigation_algorithm")
g.render(out_base, format="pdf", cleanup=True)
g.render(out_base, format="png", cleanup=True)

print(f"Saved:\n  {out_base}.pdf\n  {out_base}.png")
