#!/bin/bash
# Build the JRSS-A submission package for HMMotion from paper_jrssa/
set -e
P=/home/user/hmmotion/paper_jrssa
S=${S:-/tmp/jrssa_build}; mkdir -p $S
W=$S/submission_build; rm -rf $W; mkdir -p $W/compile $W/src
cd $P
# 1. compile the working versions to get the .bbl files
for f in main_oup supplement_oup cover_letter; do
  rm -rf $W/compile/$f; mkdir -p $W/compile/$f
  timeout 300 latexmk -pdf -interaction=nonstopmode -outdir=$W/compile/$f $f.tex >/dev/null 2>&1 || true
done
python3 - "$P" "$W" <<'PY'
import re, sys
P, W = sys.argv[1], sys.argv[2]
figmap = {"Data_preprocessing2.pdf": "Fig1_preprocessing.pdf", "HMM_expl.pdf": "Fig2_hmm_illustration.pdf",
          "snapshot.pdf": "Fig4_example_play.pdf", "all_losses_cross_fitting.pdf": "Fig5_loss_metrics.pdf",
          "gcm_test_all_plus_omnibus.pdf": "Fig6_gcm_partial.pdf", "gcm_teststat.pdf": "Fig7_gcm_marginal.pdf",
          "team_diffs.pdf": "Fig8_team_differences.pdf", "re_def.pdf": "FigS1_team_effects.pdf"}
def make(src, bbl, out, tikz_to_pdf):
    s = open(f"{P}/{src}", encoding="utf-8").read()
    def fig(m):
        name = m.group(2).split("/")[-1]
        return m.group(1) + "{" + figmap[name] + "}"
    s = re.sub(r"(\\includegraphics(?:\[[^\]]*\])?)\{([^}]*)\}", fig, s)
    if tikz_to_pdf:
        s = re.sub(r"\\begin\{tikzpicture\}.*?\\end\{tikzpicture\}", r"\\includegraphics[width=0.8\\linewidth]{Fig3_hmm_structure.pdf}", s, flags=re.S)
    b = open(bbl, encoding="utf-8").read()
    s = s.replace("\\bibliographystyle{oup-abbrvnat}\n\\bibliography{refs}", b.strip())
    assert "\\bibliography{" not in s
    s = s.replace("\\showalttextfalse", "\\showalttexttrue")
    # drop full-line comments and collapse empty lines
    lines = [l for l in s.split("\n") if not l.lstrip().startswith("%")]
    s = re.sub(r"\n{3,}", "\n\n", "\n".join(lines))
    open(f"{W}/src/{out}", "w", encoding="utf-8").write(s)
make("main_oup.tex", f"{W}/compile/main_oup/main_oup.bbl", "manuscript.tex", True)
make("supplement_oup.tex", f"{W}/compile/supplement_oup/supplement_oup.bbl", "supplementary_material.tex", False)
# figure 3 as a separate file
s = open(f"{P}/main_oup.tex", encoding="utf-8").read()
tikz = re.search(r"\\begin\{tikzpicture\}.*?\\end\{tikzpicture\}", s, re.S).group(0)
open(f"{W}/src/fig3.tex", "w").write("\\documentclass[border=4pt]{standalone}\n\\usepackage{tikz}\n\\usetikzlibrary{arrows.meta}\n\\begin{document}\n" + tikz + "\n\\end{document}\n")
import shutil
for a, b in figmap.items(): shutil.copy(f"{P}/figures/plots/{a}", f"{W}/src/{b}")
shutil.copy(f"{P}/oup-authoring-template.cls", f"{W}/src/")
PY
cd $W/src
pdflatex -interaction=nonstopmode fig3.tex >/dev/null 2>&1; mv fig3.pdf Fig3_hmm_structure.pdf; rm -f fig3.*
# 2. compile the submission files from the flat folder (no bibtex needed)
for f in manuscript supplementary_material; do
  mkdir -p $W/check_$f; cp * $W/check_$f/ 2>/dev/null; (cd $W/check_$f && for i in 1 2 3; do pdflatex -interaction=nonstopmode $f.tex >/dev/null 2>&1; done)
  echo "$f: errors $(grep -c '^!' $W/check_$f/$f.log), undefined $(grep -c 'undefined' $W/check_$f/$f.log), pages $(pdfinfo $W/check_$f/$f.pdf | grep Pages | awk '{print $2}')"
done
# 3. package
O=$W/jrssa_submission_hmmotion; mkdir -p $O/figures
cp $W/check_manuscript/manuscript.pdf $O/manuscript.pdf
cp $W/check_supplementary_material/supplementary_material.pdf $O/supplementary_material.pdf
cp $W/compile/cover_letter/cover_letter.pdf $O/cover_letter.pdf
cp $W/src/Fig*.pdf $O/figures/
(cd $W/src && zip -q $O/latex_sources.zip manuscript.tex supplementary_material.tex oup-authoring-template.cls Fig*.pdf)
rm -f $S/jrssa_submission_hmmotion.zip; (cd $W && zip -qr $S/jrssa_submission_hmmotion.zip jrssa_submission_hmmotion)
ls -la $O $O/figures; unzip -l $O/latex_sources.zip
