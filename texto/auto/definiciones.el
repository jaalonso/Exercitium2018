(TeX-add-style-hook
 "definiciones"
 (lambda ()
   (TeX-add-symbols
    '("note" 1)
    '("entrada" 1))
   (LaTeX-add-environments
    "descripcion"
    "code"))
 :latex)

