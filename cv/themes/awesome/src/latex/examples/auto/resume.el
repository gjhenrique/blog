(TeX-add-style-hook
 "resume"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("awesome-cv" "11pt" "a4paper")))
   (TeX-run-style-hooks
    "latex2e"
    "resume/education"
    "resume/experience"
    "resume/extracurricular"
    "resume/honors"
    "resume/presentation"
    "resume/writing"
    "resume/committees"
    "awesome-cv"
    "awesome-cv11"))
 :latex)

