"""Create main_oup_changes.tex: main_oup.tex with all changes relative to the
AoAS version (commit 6103143, paper_jrssa/main.tex) highlighted.
Added text in yellow, deleted text in red. Requires git and latexdiff.
Run from paper_jrssa/:  python3 make_changes.py"""
import re, subprocess

orig = subprocess.run(["git", "show", "6103143:paper_jrssa/main.tex"],
                      capture_output=True, text=True, check=True).stdout
new = open("main_oup.tex").read()

# old text in the new template: title, abstract, keywords and body of the AoAS version
ol = orig.split("\n")
def idx(lines, pat):
    r = [i for i, l in enumerate(lines) if l.startswith(pat)]
    assert len(r) == 1, (pat, r)
    return r[0]
obody = "\n".join(ol[idx(ol, "\\section{Introduction}"):idx(ol, "\\bibliographystyle{apalike}")])
oabs = re.search(r"\\begin\{abstract\}\s*\n\\noindent\n(.*?)\n\\end\{abstract\}", orig, re.S).group(1).strip()
otitle = "Integrating Unsupervised and Supervised Learning for the Prediction of Defensive Schemes in American football"
okw = re.search(r"\\keywords\{([^}]*)\}", orig).group(1)
m = re.sub(r"(\\title\[[^\]]*\]\{)[^}]*\}", lambda x: x.group(1) + otitle + "}", new, count=1)
i0, i1 = m.index("\\abstract{"), m.index("\\keywords{")
m = m[:i0] + "\\abstract{" + oabs + "}\n\n" + m[i1:]
m = re.sub(r"\\keywords\{[^}]*\}", lambda x: "\\keywords{" + okw + "}", m, count=1)
b0 = m.index("\\maketitle") + len("\\maketitle")
b1 = m.index("\\bibliographystyle{oup-abbrvnat}")
m = m[:b0] + "\n\n" + obody + "\n\n" + m[b1:]
open("old_oup_tmp.tex", "w").write(m)

d = subprocess.run(["latexdiff", "--type=UNDERLINE", "--math-markup=off",
                    "--exclude-textcmd=section,subsection,subsubsection",
                    "old_oup_tmp.tex", "main_oup.tex"],
                   capture_output=True, text=True, check=True).stdout
subprocess.run(["rm", "old_oup_tmp.tex"])

# markup: yellow highlight for additions, red for deletions
a = ("\\providecommand{\\DIFadd}[1]{{\\protect\\color{blue}\\uwave{#1}}} %DIF PREAMBLE\n"
     "\\providecommand{\\DIFdel}[1]{{\\protect\\color{red}\\sout{#1}}}                      %DIF PREAMBLE\n")
assert d.count(a) == 1
d = d.replace(a, r"""% Changes relative to the AoAS version (generated with make_changes.py): added
% text highlighted in yellow, deleted text in red; changed displayed equations
% are shown in their new form without markup
\RequirePackage{soul} %DIF PREAMBLE
\sethlcolor{yellow} %DIF PREAMBLE
\soulregister\cite7 \soulregister\citep7 \soulregister\citet7 \soulregister\citealp7 %DIF PREAMBLE
\soulregister\ref7 \soulregister\eqref7 \soulregister\url7 \soulregister\texttt7 \soulregister\emph7 \soulregister\textit7 %DIF PREAMBLE
\providecommand{\DIFadd}[1]{\ifmmode{#1}\else\hl{#1}\fi} %DIF PREAMBLE
\providecommand{\DIFdel}[1]{{\protect\color{red}#1}} %DIF PREAMBLE
""")

# make the arguments of \DIFadd / \DIFdel digestible for soul
def fix(t):
    t = re.sub(r"\\mbox\{%DIFAUXCMD\n((?:\\cite[a-z]*|\\ref|\\eqref)(?:\[[^\]]*\])*\{[^}]*\})\}\\hskip0pt%DIFAUXCMD",
               lambda x: x.group(1), t)
    t = t.replace("\\hskip0pt%DIFAUXCMD", "")
    t = t.replace(".\\ ", ".~")
    t = re.sub(r"(?<!\\)\$(.+?)(?<!\\)\$", lambda x: "\\mbox{$" + x.group(1) + "$}", t)
    return t
out, i = [], 0
pat = re.compile(r"\\DIF(add|del)(FL)?\{")
while True:
    mm = pat.search(d, i)
    if not mm:
        out.append(d[i:]); break
    out.append(d[i:mm.end()]); k, depth = mm.end(), 1
    while depth:
        c = d[k]
        if c == "\\": k += 2; continue
        depth += (c == "{") - (c == "}"); k += 1
    out.append(fix(d[mm.end():k - 1]) + "}"); i = k
d = "".join(out)

# the abstract was rewritten completely: highlight it as a whole
i = d.index("\\abstract{") + len("\\abstract{"); k, depth = i, 1
while depth:
    depth += (d[k] == "{") - (d[k] == "}"); k += 1
d = d[:i] + "\\hl{" + d[i:k - 1].replace("\u2019", "'") + "}" + d[k - 1:]

open("main_oup_changes.tex", "w").write(d)
print("written main_oup_changes.tex")
