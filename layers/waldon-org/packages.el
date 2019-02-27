;;; packages.el --- waldon-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Waldon Chen <waldon@WaldondeMBP.lan>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `waldon-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `waldon-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `waldon-org/pre-init-PACKAGE' and/or
;;   `waldon-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst waldon-org-packages
  '(
    org
    org-ref
    (ox-latex-subfigure :location local)
    (ibuffer-hydra :location local)
    ))

(defun waldon-org/post-init-org ()
  (with-eval-after-load 'org
    (progn

      (require 'org-compat)
      (require 'org)
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)

      (setq initial-major-mode 'org-mode)

      ;; "◉" "○" "✸" "◻" "❀" "✡"
      ;; "☰" "☷" "☯" "☭" "◉" "○" "✸" "✿" "■" "◆" "▲" "▶"
      (setq org-bullets-bullet-list '("☰" "☷" "☯" "☭" "◉" "○" "✸" "■" "◆" "▶"))

      (setq org-directory "~/Documents/org")
      (setq org-agenda-file-gtd (expand-file-name "task.org" org-directory))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-directory))
      (setq org-agenda-file-note (expand-file-name "notes.org" org-directory))
      (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-directory))
      (setq org-agenda-file-private (expand-file-name "private.org" org-directory))
      (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
      (setq org-agenda-files (list org-directory))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org Agenda
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (setq org-agenda-inhibit-startup t)
      (setq org-agenda-use-tag-inheritance nil)
      (setq org-agenda-span 'day)
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)

      (setq org-agenda-custom-commands
            (quote (("P" "Projects" ((tags "PROJECT")))
                    ("H" "Office and Home Lists"
                     ((agenda)
                      (tags-todo "OFFICE")
                      (tags-todo "HOME")
                      (tags-todo "COMPUTER")
                      (tags-todo "READING")))
                    ("D" "Daily Action List"
                     ((agenda "" ((org-agenda-ndays 1)
                                  (org-agenda-sorting-strategy
                                   (quote ((agenda time-up priority-down tag-up))))
                                  (org-deadline-warning-days 0)
                                  ))))
                    ("N" "Notes" tags "NOTE"
                     ((org-agenda-overriding-header "Notes")
                      (org-tags-match-list-sublevels t)))
                    ("n" "Agenda and all TODOs"
                     ((agenda #1="")
                      (tags "REFILE"
                            ((org-agenda-overriding-header "Tasks to Refile")
                             (org-tags-match-list-sublevels nil)))
                      (tags-todo "-CANCELLED/!"
                                 ((org-agenda-overriding-header "Stuck Projects")
                                  (org-agenda-skip-function 'waldon-org/skip-non-stack-projects)
                                  (org-agenda-sorting-strategy '(category-keep))))
                      (tags-todo "-HOLD-CANCELLED/!"
                                 ((org-agenda-overriding-header "Projects")
                                  (org-agenda-skip-function 'waldon-org/skip-non-stack-projects)
                                  (org-tags-match-list-sublevels 'indented)
                                  (org-agenda-sorting-strategy '(category-keep))))
                      (tags-todo "-CANCELLED/!NEXT"
                                 ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                        (if waldon-org/hide-scheduled-and-waiting-next-tasks
                                                                            ""
                                                                          " (including WAITING and SCHEDULED tasks)")))
                                  (org-agenda-skip-function 'waldon-org/skip-projects-and-single-tasks)
                                  (org-tags-match-list-sublevels t)
                                  (org-agenda-todo-ignore-scheduled waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-deadlines waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-with-date waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-sorting-strategy
                                   '(todo-state-down effort-up category-keep))))
                      (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                 ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                        (if waldon-org/hide-scheduled-and-waiting-next-tasks
                                                                            ""
                                                                          " (including WAITING and SCHEDULED tasks)")))
                                  (org-agenda-skip-function 'waldon-org/skip-non-project-tasks)
                                  (org-agenda-todo-ignore-scheduled waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-deadlines waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-with-date waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-sorting-strategy
                                   '(category-keep))))
                      (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                 ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                        (if waldon-org/hide-scheduled-and-waiting-next-tasks
                                                                            ""
                                                                          " (including WAITING and SCHEDULED tasks)")))
                                  (org-agenda-skip-function 'waldon-org/skip-project-tasks)
                                  (org-agenda-todo-ignore-scheduled waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-deadlines waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-with-date waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-sorting-strategy
                                   '(category-keep))))
                      (tags-todo "-CANCELLED+WAITING|HOLD/!"
                                 ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                        (if waldon-org/hide-scheduled-and-waiting-next-tasks
                                                                            ""
                                                                          " (including WAITING and SCHEDULED tasks)")))
                                  (org-agenda-skip-function 'waldon-org/skip-non-tasks)
                                  (org-tags-match-list-sublevels nil)
                                  (org-agenda-todo-ignore-scheduled waldon-org/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-deadlines waldon-org/hide-scheduled-and-waiting-next-tasks)))
                      (tags "-REFILE/"
                            ((org-agenda-overriding-header "Tasks to Archive")
                             (org-agenda-skip-function 'waldon-org/skip-non-archivable-tasks)
                             (org-tags-match-list-sublevels nil)))
                      (alltodo #1#))
                     ))
                   ))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org Capture
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (require 'org-capture)

      ;; configure org-capture templates
      (setq org-capture-templates nil)

      ;; 未分类内容
      (add-to-list 'org-capture-templates
                   '("i" "Inbox" entry
                     (file org-default-file-gtd)
                     "* %U - %^{标题} %^g\n  %?\n" :empty-lines 1))
      ;; 任务系统
      (add-to-list 'org-capture-templates '("t" "Tasks"))
      ;; 普通任务
      (add-to-list 'org-capture-templates
                   '("ti" "General Task" entry
                     (file+olp org-agenda-file-gtd "Inbox")
                     "* TODO %^{标题}\n  SCHEDULED: %^T DEADLINE: %^t\n\n  %?"
                     :empty-lines 1))
      ;; 书籍阅读任务
      (add-to-list 'org-capture-templates
                   '("tb" "Book Reading Task" entry
                     (file+olp org-agenda-file-gtd "Reading" "Book")
                     "* TODO %^{标题}\n  SCHEDULED: %^T DEADLINE: %^t\n\n  %?"
                     :empty-lines 1))
      ;; 论文阅读任务
      (add-to-list 'org-capture-templates
                   '("tp" "Paper Reading Task" entry
                     (file+olp org-agenda-file-gtd "Reading" "Paper")
                     "* TODO %^{标题}\n  SCHEDULED: %^T DEADLINE: %^t\n\n  %?"
                     :empty-lines 1))
      ;; 工作任务
      (add-to-list 'org-capture-templates
                   '("tw" "Work Task" entry
                     (file+olp org-agenda-file-gtd "Work")
                     "* TODO %^{标题}\n  SCHEDULED: %^T DEADLINE: %^t\n\n  %?"
                     :clock-in t :clock-resume t :empty-lines 1))
      ;; 写作任务
      (add-to-list 'org-capture-templates
                   '("tB" "Blog Writing Task" entry
                     (file+olp org-agenda-file-gtd "Knowledge" "Blog")
                     "* TODO %^{标题}\n  SCHEDULED: %^T DEADLINE: %^t\n\n  %?"
                     :empty-lines 1))
      ;; Snippets
      (add-to-list 'org-capture-templates
                   `("s" "Snippets" entry
                     (file org-agenda-file-code-snippet)
                     "* %^{标题} %t %^g\n  %?\n" :empty-lines 1))
      ;; 日记
      (add-to-list 'org-capture-templates
                   `("j" "Journal" entry
                     (file+datetree org-agenda-file-journal)
                     "* %U - %^{标题} %^g\n  %?\n" :empty-lines 1))
      ;; 账单
      (add-to-list 'org-capture-templates
                   `("b" "Billing" plain
                     (file+function org-agenda-file-private waldon-org/find-month-tree)
                     "   | %U | %^{类别} | %^{描述} | %^{金额} | %^{备注} |" :kill-buffer t))
      ;; 联系人
      (add-to-list 'org-capture-templates
                   `("c" "Contacts" entry
                     (file+headline org-agenda-file-private "Contacts")
                     "* %^{姓名} %^{手机号}p %^{邮箱}p %^{住址}p\n\n  %?" :empty-lines 1))
      ;; 约会
      (add-to-list 'org-capture-templates
                   '("m" "Meeting" entry
                     (file ord-default-notes-file)
                     "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))
      ;; 电话
      (add-to-list 'org-capture-templates
                   '("c" "Phone call" entry
                     (file org-default-notes-file)
                     "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t))
      ;; 回复
      (add-to-list 'org-capture-templates
                   '("r" "Respond" entry
                     (file org-default-notes-file)
                     "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
                     :clock-in t :clock-resume t :immediate-finish t))

      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets
            '((nil :maxlevel . 4)
              (org-agenda-files :maxlevel . 4)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org Clock
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

      (setq org-tags-match-list-sublevels nil)

      (add-hook 'org-mode-hook '(lambda ()
                                  ;; keybinding for editing source code blocks
                                  ;; keybinding for inserting code blocks
                                  (local-set-key (kbd "C-c i s")
                                                 'zilongshanren/org-insert-src-block)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org Archive
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (setq org-archive-location "%s_archive::date-tree")

      (defadvice org-archive-subtree
          (around org-archive-subtree-to-data-tree activate)
        "org-archive-subtree to date-tree"
        (if
            (string= "date-tree"
                     (org-extract-archive-heading
                      (org-get-local-archive-location)))
            (let* ((dct (decode-time (org-current-time)))
                   (y (nth 5 dct))
                   (m (nth 4 dct))
                   (d (nth 3 dct))
                   (this-buffer (current-buffer))
                   (location (org-get-local-archive-location))
                   (afile (org-extract-archive-file location))
                   (org-archive-location
                    (format "%s::*** %04d-%02d-%02d %s" afile y m d
                            (format-time-string "%A" (encode-time 0 0 0 d m y)))))
              (message "afile=%s" afile)
              (unless afile
                (error "Invalid `org-archive-location'"))
              (save-excursion
                (switch-to-buffer (find-file-noselect afile))
                ;; (org-datetree-find-year-create y)
                ;; (org-datetree-find-month-create y m)
                ;; (org-datetree-find-day-create y m d)
                (widen)
                (switch-to-buffer this-buffer))
              ad-do-it)
          ad-do-it))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Org Publish
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (require 'ox-publish)

      (setq org-latex-classes nil)
      (add-to-list 'org-latex-classes
	                 '("beamer"
		                 "\\documentclass[presentation]{beamer}"
		                 ("\\section{%s}" . "\\section*{%s}")
		                 ("\\subsection{%s}" . "\\subsection*{%s}")
		                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
      (add-to-list 'org-latex-classes
                   '("article" "\\documentclass[11pt]{article}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      (add-to-list 'org-latex-classes
                   '("report" "\\documentclass[11pt]{report}"
                     ("\\part{%s}" . "\\part*{%s}")
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
      (add-to-list 'org-latex-classes
                   '("book" "\\documentclass[11pt]{book}"
                     ("\\part{%s}" . "\\part*{%s}")
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
      (add-to-list 'org-latex-classes
                   '("ctexart" "\\documentclass[11pt]{ctexart}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      (add-to-list 'org-latex-classes
                   '("ctexrep" "\\documentclass[11pt]{ctexrep}"
                     ("\\part{%s}" . "\\part*{%s}")
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
      (add-to-list 'org-latex-classes
                   '("ctexbook" "\\documentclass[11pt]{ctexbook}"
                     ("\\part{%s}" . "\\part*{%s}")
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
      (add-to-list 'org-latex-classes
                   `("ustcthesis" ,(concat "\\documentclass{ustcthesis}\n"
                                           "[NO-DEFAULT-PACKAGES]\n"
                                           "[EXTRA]\n"
                                           "[PACKAGES]\n")
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      (add-to-list 'org-latex-classes
                   '("IEEEtran" "\\documentclass[]{IEEEtran}\n"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      (add-to-list 'org-latex-classes
                   '("cn-article" "\\documentclass[11pt]{ctexart}
[NO-DEFAULT-PACKAGES]
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{booktabs}
\\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
\\tolerance=1000
\\usepackage{listings}
\\usepackage{xcolor}
\\lstset{
  %行号
  numbers=left,
  %背景框
  framexleftmargin=10mm,
  frame=lines,
  %背景色
  backgroundcolor=\\color[RGB]{245,245,244},
  %样式
  keywordstyle=\\bf\\color{blue},
  identifierstyle=\\bf,
  numberstyle=\\color[RGB]{0,192,192},
  commentstyle=\\it\\color[RGB]{0,96,96},
  stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
  %显示空格
  showstringspaces=false
}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (setq org-latex-default-class "ctexart")
      (setq org-latex-pdf-process
            '("latexmk -pdflatex='%latex  --shell-escape -interaction nonstopmode -synctex=1' -pdf -bibtex -outdir='%o' -f %f"))

      ;; use xelatex to compile babel
      (setq org-format-latex-header "% xelatex
\\documentclass[12pt]{article}
\\usepackage[usenames]{color}
\[PACKAGES]
\[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

      (setq org-latex-listings t)

      ;;(setq org-plantuml-jar-path (expand-file-name "~/.spacemacs.d/plantuml.jar"))
      ;;(setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (python . t)
         (shell . t)
         (latex .t)
         (dot . t)
         (org . t)
         (C . t)
         (perl . nil)
         (ruby . nil)
         (js . nil)
         (plantuml . nil)
         (ditaa . nil)))

      (setq org-confirm-babel-evaluate nil)

      (require 'ox-extra)
      (ox-extras-activate '(ignore-headlines))

      (add-hook 'org-export-before-parsing-hook
                'waldon-org/org-latex-header-blocks-filter)
      )
    )
  )

(defun waldon-org/post-init-org-ref ()
  (use-package org-ref
    :init
    (setq org-ref-default-bibliography
          (remove-if-not (lambda (filename)
                           (equal "bib" (file-name-extension filename)))
                         (directory-files-recursively "~/Documents/Papers/"
                                                      directory-files-no-dot-files-regexp))
          org-ref-pdf-directory "~/Documents/Papers/"
          org-ref-bibliography-notes "~/Documents/Papers/notes.org")
    ))

(defun waldon-org/init-ox-latex-subfigure ()
  (use-package ox-latex-subfigure
    :init
    (setq org-latex-prefer-user-labels t)
    :config (require 'ox-latex-subfigure)))

(defun waldon-org/init-ibuffer-hydra ()
  (use-package ibuffer-hydra
    :after (ibuffer)
    :init
    (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)
    :config
    ;(require 'ibuffer-hydra)
    (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
    ))
;;; packages.el ends here
