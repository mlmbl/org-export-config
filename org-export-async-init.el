;; Set load paths to the system's emacs initializing directory.
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Load required packages
(require 'ox-latex)
(require 'citeproc)
(require 'oc-basic)
(require 'oc-csl)
(require 'oc-biblatex)
(require 'oc-natbib)

;; basic export settings
(setq org-export-with-smart-quotes t)
(setq org-export-with-broken-links 'mark) 
(setq org-export-with-toc nil) 
(setq org-export-allow-bind-keywords t)
(setq org-export-headline-levels 4)

(setq org-latex-text-markup-alist '((bold . "\\textbf{%s}")
                                    (code . protectedtexttt)
                                    (italic . "\\textit{%s}")
                                    (strike-through . "\\sout{%s}")
                                    (underline . "\\uline{%s}")
                                    (verbatim . "\\verb|%s|")))


;; org-babel settings
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)		      
   (latex . t)
   (org . t)
   (python . t)
   (R . t)
   (ruby . t) 
   (sql . t)
   (sqlite . t)))

;; citation setting
(setq org-cite-global-bibliography
      (list (expand-file-name "references.bib" default-directory)))
(setq org-cite-export-processors
      '((latex biblatex)
	))


;; latex compiler and pdf process setting
(setq org-latex-compiler "lualatex")
(setq org-latex-pdf-process '("latexmk -f %f"))

;; Title export setting
(setq org-latex-subtitle-separate nil)
(setq org-latex-subtitle-format ": %s")
(setq org-latex-title-command "
\\sffamily
\\hrule width \\linewidth height 1.5pt
\\vspace{1pt}
\\hrule width \\linewidth height 0.5pt
\\vspace{1em}
\\noindent
{\\Large \\textbf{%t}}\\\\
\\smallskip
{\\large \\textbf{%s}}\\par
\\vspace{1em}
\\noindent
%a\\par
\\vspace{0.25em}
\\begin{flushright}
{\\footnotesize %D @%d}
\\end{flushright}
\\vspace{0.25em}
\\hrule width \\linewidth height 0.5pt
\\vspace{1pt}
\\hrule width \\linewidth height 1.5pt
\\vspace{0.5em}
\\noindent
{\\small \\textbf{Keywords: }%k}
\\vspace{2em}
\\rmfamily")


;; default packages
(setq org-latex-default-packages-alist '(("AUTO" "inputenc" t ("pdflatex"))
                                         ("T1" "fontenc" t ("pdflatex"))
                                         ("" "graphicx" t)
                                         ("" "longtable" nil)
                                         ("" "wrapfig2" nil)
                                         ("" "rotating" nil)
                                         ("normalem" "ulem" t)
                                         ("" "amsmath" t)
                                         ("" "amssymb" t)
                                         ("" "capt-of" nil)
                                         ("" "hyperref" nil)
					 ("" "listings" nil)
					 ("" "booktabs" nil)
					 ("" "ltablex" nil)
					 ("" "caption" t)
					 ("twoside, top=20truemm, bottom=20truemm, inner=25truemm, outer=20truemm" "geometry" nil)
					 ("inline" "enumitem" nil)
					 ("dvipsnames" "xcolor" t)
					 ("" "jvlisting" t)
					 ("" "titlesec" t)
					 ("" "abstract" t)
					 ("" "float" t)))

;; code blocks 
(setq org-latex-listings t)
(setq org-latex-listings-options
      '(("basicstyle" "\\ttfamily\\small")
        ("keywordstyle" "\\color{blue}\\bfseries")
        ("breaklines" "true")
        ("commentstyle" "\\color{gray}\\textit")
        ("frame" "shadowbox")
        ("framesep" "0.5em")
        ("xleftmargin" "3em")
        ("xrightmargin" "3em")
        ("numbers" "left")
        ("numberstyle" "\\tiny\\color{gray}")
        ("stepnumber" "1")))


;; hyperref settings
(setq org-latex-hyperref-template "
\\hypersetup{setpagesize = false,
             pdfauthor={%a},
             pdftitle={%t},
             pdfkeywords={%k},
             pdfsubject={%d},
             pdfcreator={%c}, 
             pdflang={%L},
             colorlinks  = true,
             linkcolor   = OliveGreen,
             urlcolor    = Blue,
             filecolor   = Red,
             citecolor   = Maroon,
             bookmarks   = true}
")

;; class setting
(setq org-latex-classes nil)
(add-to-list 'org-latex-classes
	     '("lecture-handout"
               "\\documentclass[11pt, a4paper]{ltjsarticle}
\\usepackage[deluxe,jis2004,hiragino-pron]{luatexja-preset}
\\setmainfont[Ligatures=TeX, Scale=0.95]{TeX Gyre Pagella}
\\setsansfont[Ligatures=TeX, Scale=0.95]{TeX Gyre Adventor}
\\setmonofont[Ligatures=TeX, Scale=1]{TeX Gyre Cursor}
[DEFAULT-PACKAGES]
\\titleformat{\\section}
  [block]
  {\\large\\bfseries\\sffamily}
  {\\thesection}               
  {0.5em}                     
  {}
\\titlespacing{\\section}
  {0pt}
  {0.1em}
  {0.25em}
\\titleformat{\\subsection}
  [block]
  {\\normalsize\\bfseries\\sffamily}  
  {\\thesubsection}               
  {0.5em}                     
  {}
\\titlespacing{\\subsection}
  {0pt}
  {0.75em}
  {0.20em}
\\titleformat{\\subsubsection}
  [block]
  {\\normalsize\\sffamily}
  {\\thesubsubsection} 
  {0.5em}              
  {}
\\titlespacing{\\subsubsection}
  {0pt}
  {0.5em}
  {0.20em}
\\titleformat{\\paragraph}
  [runin]  
  {\\sffamily}
  {\\theparagraph}
  {1em}
  {}                                 
\\titlespacing{\\paragraph}
  {0pt}
  {0.5em}
  {1em}
\\floatstyle{plaintop}
\\restylefloat{table}
\\urlstyle{sf}
\\renewcommand\\lstlistingname{Code}
\\renewcommand{\\abstractname}{Abstract}
[EXTRA]
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq org-latex-default-class "lecture-handout")
