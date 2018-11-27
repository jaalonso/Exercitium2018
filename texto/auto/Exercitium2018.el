(TeX-add-style-hook
 "Exercitium2018"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "a4paper" "12pt" "twoside")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "spanish") ("hyperref" "colorlinks=true" "urlcolor=blue" "pdfauthor={José A. Alonso <jalonso@us.es>}" "pdftitle={Exercitium (curso 2018-19)}" "pdfstartview=FitH" "bookmarks=false")))
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "definiciones"
    "licenciaCC"
    "book"
    "bk12"
    "fontspec"
    "xltxtra"
    "babel"
    "a4wide"
    "minted"
    "comment"
    "titletoc"
    "hyperref"
    "tocstyle"
    "fancyhdr")
   (TeX-add-symbols
    "mtctitle"))
 :latex)

