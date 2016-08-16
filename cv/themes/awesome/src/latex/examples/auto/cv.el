(TeX-add-style-hook
 "cv"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("awesome-cv" "11pt" "a4paper")))
   (TeX-run-style-hooks
    "latex2e"
    "cv/education"
    "cv/skills"
    "cv/experience"
    "cv/extracurricular"
    "cv/honors"
    "cv/presentation"
    "cv/writing"
    "cv/committees"
    "awesome-cv"
    "awesome-cv11"))
 :latex)

