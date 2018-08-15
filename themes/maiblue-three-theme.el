;;; maiblue-three-theme.el --- MaiBlue v3 color theme

;; Copyright 2018, MaiHD

;; Author: MaiHD
;; URL: https://github.com/maihd/maimacs.git
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs port of the Atom One Dark theme from Atom.io.

;;; Code:

(deftheme maiblue-three-theme
  "MaiBlue Three theme - An Emacs theme use with MaiMacs.")

(defvar maiblue-three-colors-alist
  (let* ((256color  (eq (display-color-cells (selected-frame)) 256))
         (colors `(("maiblue-three-accent"   . "#528BFF")
                   ("maiblue-three-fg"       . (if ,256color "color-248" "#ABB2BF"))
                   ("maiblue-three-bg"       . (if ,256color "color-235" "#282C34"))
                   ("maiblue-three-bg-1"     . (if ,256color "color-234" "#121417"))
                   ("maiblue-three-bg-hl"    . (if ,256color "color-236" "#2C323C"))
                   ("maiblue-three-gutter"   . (if ,256color "color-239" "#4B5363"))
                   ("maiblue-three-mono-1"   . (if ,256color "color-248" "#ABB2BF"))
                   ("maiblue-three-mono-2"   . (if ,256color "color-244" "#828997"))
                   ("maiblue-three-mono-3"   . (if ,256color "color-240" "#5C6370"))
                   ("maiblue-three-cyan"     . "#56B6C2")
                   ("maiblue-three-blue"     . "#61AFEF")
                   ("maiblue-three-purple"   . "#C678DD")
                   ("maiblue-three-green"    . "#98C379")
                   ("maiblue-three-red-1"    . "#E06C75")
                   ("maiblue-three-red-2"    . "#BE5046")
                   ("maiblue-three-orange-1" . "#D19A66")
                   ("maiblue-three-orange-2" . "#E5C07B")
                   ("maiblue-three-gray"     . (if ,256color "color-237" "#3E4451"))
                   ("maiblue-three-silver"   . (if ,256color "color-247" "#9DA5B4"))
                   ("maiblue-three-black"    . (if ,256color "color-233" "#21252B"))
                   ("maiblue-three-border"   . (if ,256color "color-232" "#181A1F")))))
    colors)
  "List of MaiBlue There colors.")

(defmacro maiblue-three-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    maiblue-three-colors-alist))
     ,@body))

(maiblue-three-with-color-variables
  (custom-theme-set-faces
   'maiblue-three-theme

   `(default ((t (:foreground ,maiblue-three-fg :background ,maiblue-three-bg))))
   `(success ((t (:foreground ,maiblue-three-green))))
   `(warning ((t (:foreground ,maiblue-three-orange-2))))
   `(error ((t (:foreground ,maiblue-three-red-1 :weight bold))))
   `(link ((t (:foreground ,maiblue-three-blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,maiblue-three-blue :underline t :weight normal))))
   `(cursor ((t (:background ,maiblue-three-accent))))
   `(fringe ((t (:background ,maiblue-three-bg))))
   `(region ((t (:background ,maiblue-three-gray))))
   `(highlight ((t (:background ,maiblue-three-gray))))
   `(hl-line ((t (:background ,maiblue-three-bg-hl))))
   `(vertical-border ((t (:background ,maiblue-three-border :foreground ,maiblue-three-border))))
   `(secondary-selection ((t (:background ,maiblue-three-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,maiblue-three-silver))))

   `(font-lock-builtin-face ((t (:foreground ,maiblue-three-blue))))
   `(font-lock-comment-face ((t (:foreground ,maiblue-three-mono-3))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,maiblue-three-blue))))
   `(font-lock-keyword-face ((t (:foreground ,maiblue-three-purple :weight normal))))
   `(font-lock-preprocessor-face ((t (:foreground ,maiblue-three-blue))))
   `(font-lock-string-face ((t (:foreground ,maiblue-three-green))))
   `(font-lock-type-face ((t (:foreground ,maiblue-three-orange-2))))
   `(font-lock-constant-face ((t (:foreground ,maiblue-three-orange-1))))
   `(font-lock-variable-name-face ((t (:foreground ,maiblue-three-red-1))))
   `(font-lock-warning-face ((t (:foreground ,maiblue-three-mono-3 :bold t))))

   ;; mode-line
   `(mode-line ((t (:background ,maiblue-three-black :foreground ,maiblue-three-silver :box (:color ,maiblue-three-border :line-width 1)))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,maiblue-three-border :foreground ,maiblue-three-gray :box (:color ,maiblue-three-border :line-width 1)))))

   ;; ido
   `(ido-first-match ((t (:foreground ,maiblue-three-purple :weight bold))))
   `(ido-only-match ((t (:foreground ,maiblue-three-red-1 :weight bold))))
   `(ido-subdir ((t (:foreground ,maiblue-three-blue))))
   `(ido-virtual ((t (:foreground ,maiblue-three-mono-3))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,maiblue-three-mono-3 :background ,maiblue-three-bg-1 :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,maiblue-three-red-1 :background ,maiblue-three-bg-1 :inverse-video nil))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,maiblue-three-fg :background ,maiblue-three-bg-1))))
   `(company-tooltip-annotation ((t (:foreground ,maiblue-three-mono-2 :background ,maiblue-three-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,maiblue-three-fg :background ,maiblue-three-gray))))
   `(company-tooltip-mouse ((t (:background ,maiblue-three-gray))))
   `(company-tooltip-common ((t (:foreground ,maiblue-three-orange-2 :background ,maiblue-three-bg-1))))
   `(company-tooltip-common-selection ((t (:foreground ,maiblue-three-orange-2 :background ,maiblue-three-gray))))
   `(company-preview ((t (:background ,maiblue-three-bg))))
   `(company-preview-common ((t (:foreground ,maiblue-three-orange-2 :background ,maiblue-three-bg))))
   `(company-scrollbar-fg ((t (:background ,maiblue-three-mono-1))))
   `(company-scrollbar-bg ((t (:background ,maiblue-three-bg-1))))

   ;; compilation
   `(compilation-face ((t (:foreground ,maiblue-three-fg))))
   `(compilation-line-number ((t (:foreground ,maiblue-three-mono-2))))
   `(compilation-column-number ((t (:foreground ,maiblue-three-mono-2))))

   ;; isearch
   `(isearch ((t (:foreground ,maiblue-three-bg :background ,maiblue-three-purple))))
   `(isearch-fail ((t (:foreground ,maiblue-three-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,maiblue-three-purple :background ,maiblue-three-bg-1 :underline ,maiblue-three-purple))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#FD5FF1"))))

   ;; helm
   `(helm-header ((t (:foreground ,maiblue-three-mono-2
                      :background ,maiblue-three-bg
                      :underline nil
                      :box (:line-width 6 :color ,maiblue-three-bg)))))
   `(helm-source-header ((t (:foreground ,maiblue-three-orange-2
                             :background ,maiblue-three-bg
                             :underline nil
                             :weight bold
                             :box (:line-width 6 :color ,maiblue-three-bg)))))
   `(helm-selection ((t (:background ,maiblue-three-gray))))
   `(helm-selection-line ((t (:background ,maiblue-three-gray))))
   `(helm-visible-mark ((t (:foreground ,maiblue-three-bg :foreground ,maiblue-three-orange-2))))
   `(helm-candidate-number ((t (:foreground ,maiblue-three-green :background ,maiblue-three-bg-1))))
   `(helm-separator ((t (:background ,maiblue-three-bg :foreground ,maiblue-three-red-1))))
   `(helm-M-x-key ((t (:foreground ,maiblue-three-orange-1))))
   `(helm-bookmark-addressbook ((t (:foreground ,maiblue-three-orange-1))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,maiblue-three-purple))))
   `(helm-bookmark-info ((t (:foreground ,maiblue-three-green))))
   `(helm-bookmark-man ((t (:foreground ,maiblue-three-orange-2))))
   `(helm-bookmark-w3m ((t (:foreground ,maiblue-three-purple))))
   `(helm-match ((t (:foreground ,maiblue-three-orange-2))))
   `(helm-ff-directory ((t (:foreground ,maiblue-three-cyan :background ,maiblue-three-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,maiblue-three-fg :background ,maiblue-three-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,maiblue-three-green :background ,maiblue-three-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,maiblue-three-red-1 :background ,maiblue-three-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,maiblue-three-orange-2 :background ,maiblue-three-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,maiblue-three-bg :background ,maiblue-three-orange-2 :weight normal))))
   `(helm-buffer-not-saved ((t (:foreground ,maiblue-three-red-1))))
   `(helm-buffer-process ((t (:foreground ,maiblue-three-mono-2))))
   `(helm-buffer-saved-out ((t (:foreground ,maiblue-three-fg))))
   `(helm-buffer-size ((t (:foreground ,maiblue-three-mono-2))))
   `(helm-buffer-directory ((t (:foreground ,maiblue-three-purple))))
   `(helm-grep-cmd-line ((t (:foreground ,maiblue-three-cyan))))
   `(helm-grep-file ((t (:foreground ,maiblue-three-fg))))
   `(helm-grep-finish ((t (:foreground ,maiblue-three-green))))
   `(helm-grep-lineno ((t (:foreground ,maiblue-three-mono-2))))
   `(helm-grep-finish ((t (:foreground ,maiblue-three-red-1))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-swoop-target-line-block-face ((t (:background ,maiblue-three-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-line-face ((t (:background ,maiblue-three-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-word-face ((t (:background ,maiblue-three-purple :foreground "#ffffff"))))
   `(helm-locate-finish ((t (:foreground ,maiblue-three-green))))
   `(info-menu-star ((t (:foreground ,maiblue-three-red-1))))

   ;; ivy
   `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,maiblue-three-green))))
   `(ivy-current-match ((t (:background ,maiblue-three-gray :weight normal))))
   `(ivy-highlight-face ((t (:inherit font-lock-builtin-face))))
   `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,maiblue-three-red-1))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,maiblue-three-bg-hl))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background ,maiblue-three-black :foreground ,maiblue-three-purple :weight semi-bold))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2 :background ,maiblue-three-black :foreground ,maiblue-three-green :weight semi-bold))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2 :background ,maiblue-three-black :foreground ,maiblue-three-orange-2 :weight semi-bold))))
   `(ivy-minibuffer-match-highlight ((t (:foreground ,maiblue-three-blue))))
   `(ivy-modified-buffer ((t (:inherit default :foreground ,maiblue-three-orange-1))))
   `(ivy-virtual ((t (:inherit font-lock-builtin-face :slant italic))))

   ;; counsel
   `(counsel-key-binding ((t (:foreground ,maiblue-three-orange-2 :weight bold))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,maiblue-three-green :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,maiblue-three-blue :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,maiblue-three-orange-2 :weight bold))))

   ;; jabber
   `(jabber-roster-user-online ((t (:foreground ,maiblue-three-green))))
   `(jabber-roster-user-away ((t (:foreground ,maiblue-three-red-1))))
   `(jabber-roster-user-xa ((t (:foreground ,maiblue-three-red-2))))
   `(jabber-roster-user-dnd ((t (:foreground ,maiblue-three-purple))))
   `(jabber-roster-user-chatty ((t (:foreground ,maiblue-three-orange-2))))
   `(jabber-roster-user-error ((t (:foreground ,maiblue-three-red-1 :bold t))))
   `(jabber-roster-user-offline ((t (:foreground ,maiblue-three-mono-3))))
   `(jabber-chat-prompt-local ((t (:foreground ,maiblue-three-blue))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,maiblue-three-orange-2))))
   `(jabber-chat-prompt-system ((t (:foreground ,maiblue-three-mono-3))))
   `(jabber-chat-error ((t (:inherit jabber-roster-user-error))))
   `(jabber-rare-time-face ((t (:foreground ,maiblue-three-cyan))))
   `(jabber-activity-face ((t (:inherit jabber-chat-prompt-foreign))))
   `(jabber-activity-personal-face ((t (:inherit jabber-chat-prompt-local))))

   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,maiblue-three-mono-1))))
   `(js2-jsdoc-tag ((t (:foreground ,maiblue-three-purple))))
   `(js2-jsdoc-type ((t (:foreground ,maiblue-three-orange-2))))
   `(js2-jsdoc-value((t (:foreground ,maiblue-three-red-1))))
   `(js2-object-property ((t (:foreground ,maiblue-three-red-1))))

   ;; magit
   `(magit-section-highlight ((t (:background ,maiblue-three-bg-hl))))
   `(magit-section-heading ((t (:foreground ,maiblue-three-orange-2 :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,maiblue-three-fg :weight bold))))
   `(magit-diff-file-heading ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,maiblue-three-gray :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,maiblue-three-orange-2 :background ,maiblue-three-bg-hl :weight bold))))
   `(magit-diff-hunk-heading ((t (:foreground ,maiblue-three-mono-2 :background ,maiblue-three-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,maiblue-three-mono-1 :background ,maiblue-three-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,maiblue-three-purple :background ,maiblue-three-mono-3))))
   `(magit-diff-context ((t (:foreground ,maiblue-three-fg))))
   `(magit-diff-context-highlight ((t (:background ,maiblue-three-bg-1 :foreground ,maiblue-three-fg))))
   `(magit-diffstat-added ((t (:foreground ,maiblue-three-green))))
   `(magit-diffstat-removed ((t (:foreground ,maiblue-three-red-1))))
   `(magit-process-ok ((t (:foreground ,maiblue-three-green))))
   `(magit-process-ng ((t (:foreground ,maiblue-three-red-1))))
   `(magit-log-author ((t (:foreground ,maiblue-three-orange-2))))
   `(magit-log-date ((t (:foreground ,maiblue-three-mono-2))))
   `(magit-log-graph ((t (:foreground ,maiblue-three-silver))))
   `(magit-sequence-pick ((t (:foreground ,maiblue-three-orange-2))))
   `(magit-sequence-stop ((t (:foreground ,maiblue-three-green))))
   `(magit-sequence-part ((t (:foreground ,maiblue-three-orange-1))))
   `(magit-sequence-head ((t (:foreground ,maiblue-three-blue))))
   `(magit-sequence-drop ((t (:foreground ,maiblue-three-red-1))))
   `(magit-sequence-done ((t (:foreground ,maiblue-three-mono-2))))
   `(magit-sequence-onto ((t (:foreground ,maiblue-three-mono-2))))
   `(magit-bisect-good ((t (:foreground ,maiblue-three-green))))
   `(magit-bisect-skip ((t (:foreground ,maiblue-three-orange-1))))
   `(magit-bisect-bad ((t (:foreground ,maiblue-three-red-1))))
   `(magit-blame-heading ((t (:background ,maiblue-three-bg-1 :foreground ,maiblue-three-mono-2))))
   `(magit-blame-hash ((t (:background ,maiblue-three-bg-1 :foreground ,maiblue-three-purple))))
   `(magit-blame-name ((t (:background ,maiblue-three-bg-1 :foreground ,maiblue-three-orange-2))))
   `(magit-blame-date ((t (:background ,maiblue-three-bg-1 :foreground ,maiblue-three-mono-3))))
   `(magit-blame-summary ((t (:background ,maiblue-three-bg-1 :foreground ,maiblue-three-mono-2))))
   `(magit-dimmed ((t (:foreground ,maiblue-three-mono-2))))
   `(magit-hash ((t (:foreground ,maiblue-three-purple))))
   `(magit-tag  ((t (:foreground ,maiblue-three-orange-1 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,maiblue-three-green :weight bold))))
   `(magit-branch-local   ((t (:foreground ,maiblue-three-blue :weight bold))))
   `(magit-branch-current ((t (:foreground ,maiblue-three-blue :weight bold :box t))))
   `(magit-head           ((t (:foreground ,maiblue-three-blue :weight bold))))
   `(magit-refname        ((t (:background ,maiblue-three-bg :foreground ,maiblue-three-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,maiblue-three-bg :foreground ,maiblue-three-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,maiblue-three-bg :foreground ,maiblue-three-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,maiblue-three-green))))
   `(magit-signature-bad       ((t (:foreground ,maiblue-three-red-1))))
   `(magit-signature-untrusted ((t (:foreground ,maiblue-three-orange-1))))
   `(magit-cherry-unmatched    ((t (:foreground ,maiblue-three-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,maiblue-three-purple))))
   `(magit-reflog-commit       ((t (:foreground ,maiblue-three-green))))
   `(magit-reflog-amend        ((t (:foreground ,maiblue-three-purple))))
   `(magit-reflog-merge        ((t (:foreground ,maiblue-three-green))))
   `(magit-reflog-checkout     ((t (:foreground ,maiblue-three-blue))))
   `(magit-reflog-reset        ((t (:foreground ,maiblue-three-red-1))))
   `(magit-reflog-rebase       ((t (:foreground ,maiblue-three-purple))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,maiblue-three-green))))
   `(magit-reflog-remote       ((t (:foreground ,maiblue-three-cyan))))
   `(magit-reflog-other        ((t (:foreground ,maiblue-three-cyan))))

   ;; perspective
   `(persp-selected-face ((t (:foreground ,maiblue-three-blue))))

   ;; powerline
   `(powerline-active1 ((,class (:background ,maiblue-three-bg-hl :foreground ,maiblue-three-purple))))
   `(powerline-active2 ((,class (:background ,maiblue-three-bg-hl :foreground ,maiblue-three-purple))))
   `(powerline-inactive1 ((,class (:background ,maiblue-three-bg :foreground ,maiblue-three-fg))))
   `(powerline-inactive2 ((,class (:background ,maiblue-three-bg :foreground ,maiblue-three-fg))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,maiblue-three-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,maiblue-three-purple))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,maiblue-three-blue))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,maiblue-three-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,maiblue-three-green))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,maiblue-three-orange-1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,maiblue-three-orange-2))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,maiblue-three-red-1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,maiblue-three-red-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,maiblue-three-mono-1))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,maiblue-three-mono-2))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,maiblue-three-mono-3))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,maiblue-three-black))))

   ;; rbenv
   `(rbenv-active-ruby-face ((t (:foreground ,maiblue-three-green))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,maiblue-three-red-1 :background ,maiblue-three-gray :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,maiblue-three-gray :weight bold))))

   ;; spaceline
   `(spaceline-flycheck-error  ((,class (:foreground ,maiblue-three-red-1))))
   `(spaceline-flycheck-info   ((,class (:foreground ,maiblue-three-green))))
   `(spaceline-flycheck-warning((,class (:foreground ,maiblue-three-orange-1))))
   `(spaceline-python-venv ((,class (:foreground ,maiblue-three-purple))))

   ;; web-mode
   `(web-mode-symbol-face ((t (:foreground ,maiblue-three-orange-1))))

   ;; flx-ido
   `(flx-highlight-face ((t (:inherit (link) :weight bold))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:foreground ,maiblue-three-blue))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground "#FFFFFF" :background ,maiblue-three-red-2))))
   `(rpm-spec-macro-face ((t (:foreground ,maiblue-three-orange-2))))
   `(rpm-spec-var-face ((t (:foreground ,maiblue-three-red-1))))
   `(rpm-spec-doc-face ((t (:foreground ,maiblue-three-purple))))
   `(rpm-spec-dir-face ((t (:foreground ,maiblue-three-cyan))))
   `(rpm-spec-package-face ((t (:foreground ,maiblue-three-red-2))))
   `(rpm-spec-ghost-face ((t (:foreground ,maiblue-three-red-2))))
   `(rpm-spec-section-face ((t (:foreground ,maiblue-three-orange-2))))

   ;; term
   `(term-color-black ((t :foreground ,maiblue-three-mono-1)))
   `(term-color-blue ((t (:foreground ,maiblue-three-blue))))
   `(term-color-cyan ((t :foreground ,maiblue-three-cyan)))
   `(term-color-green ((t (:foreground ,maiblue-three-green))))
   `(term-color-magenta ((t :foreground ,maiblue-three-purple)))
   `(term-color-red ((t :foreground ,maiblue-three-red-1)))
   `(term-color-white ((t :foreground ,maiblue-three-fg)))
   `(term-color-yellow ((t (:foreground ,maiblue-three-orange-1))))

   ;; linum
   `(linum ((t (:foreground ,maiblue-three-gutter :background ,maiblue-three-bg))))
   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,maiblue-three-fg :background ,maiblue-three-bg))))
   ;; native line numbers (emacs version >=26)
   `(line-number ((t (:foreground ,maiblue-three-gutter :background ,maiblue-three-bg))))
   `(line-number-current-line ((t (:foreground ,maiblue-three-fg :background ,maiblue-three-bg))))

   ;; latex-mode
   `(font-latex-sectioning-0-face ((t (:foreground ,maiblue-three-blue :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:foreground ,maiblue-three-blue :height 1.0))))
   `(font-latex-sectioning-2-face ((t (:foreground ,maiblue-three-blue :height 1.0))))
   `(font-latex-sectioning-3-face ((t (:foreground ,maiblue-three-blue :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:foreground ,maiblue-three-blue :height 1.0))))
   `(font-latex-sectioning-5-face ((t (:foreground ,maiblue-three-blue :height 1.0))))
   `(font-latex-bold-face ((t (:foreground ,maiblue-three-green :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,maiblue-three-green))))
   `(font-latex-warning-face ((t (:foreground ,maiblue-three-red-1))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,maiblue-three-cyan))))

   ;; org-mode
   `(org-date ((t (:foreground ,maiblue-three-cyan))))
   `(org-footnote ((t (:foreground ,maiblue-three-cyan))))
   `(org-sexp-date ((t (:foreground ,maiblue-three-cyan))))

   ;; realgud
   `(realgud-overlay-arrow1        ((t (:foreground ,maiblue-three-green))))
   `(realgud-overlay-arrow3        ((t (:foreground ,maiblue-three-orange-1))   `(realgud-overlay-arrow2        ((t (:foreground ,maiblue-three-orange-2))))
))
   '(realgud-bp-enabled-face       ((t (:inherit (error)))))
   `(realgud-bp-disabled-face      ((t (:inherit (secondary-selection)))))
   `(realgud-bp-line-enabled-face  ((t (:box (:color ,maiblue-three-red-1)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color ,maiblue-three-gray)))))
   `(realgud-line-number           ((t (:foreground ,maiblue-three-mono-2))))
   `(realgud-backtrace-number      ((t (:inherit (secondary-selection)))))

   ;; undo-tree
   `(undo-tree-visualizer-current-face ((t (:foreground ,maiblue-three-red-1))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,maiblue-three-orange-1))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,maiblue-three-cyan))))
   ))

(maiblue-three-with-color-variables
  (custom-theme-set-variables
   'maiblue-three-theme
;;;;; fill-column-indicator
   `(fci-rule-color ,maiblue-three-gray)
   ))

(defvar maiblue-three-theme-force-faces-for-mode t
  "If t, maiblue-three-theme will use Face Remapping to alter the theme faces for
the current buffer based on its mode in an attempt to mimick the Atom One Dark
Theme from Atom.io as best as possible.
The reason this is required is because some modes (html-mode, jyaml-mode, ...)
do not provide the necessary faces to do theming without conflicting with other
modes.
Current modes, and their faces, impacted by this variable:
* js2-mode: font-lock-constant-face, font-lock-doc-face, font-lock-variable-name-face
")

;; Many modes in Emacs do not define their own faces and instead use standard Emacs faces when it comes to theming.
;; That being said, to have a real "Atom One Dark Theme" for Emacs, we need to work around this so that these themes look
;; as much like "MaiBlue Theme Theme" as possible.  This means using per-buffer faces via "Face Remapping":
;;
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
;;
;; Of course, this might be confusing to some when in one mode they see keywords highlighted in one face and in another
;; mode they see a different face.  That being said, you can set the `maiblue-three-theme-force-faces-for-mode` variable to
;; `nil` to disable this feature.
(defun maiblue-three-theme-change-faces-for-mode ()
  (interactive)
  (and (eq maiblue-three-theme-force-faces-for-mode t)
       (cond
        ((member major-mode '(js2-mode))
         ;; maiblue-three-orange-1
         (face-remap-add-relative 'font-lock-constant-face :foreground "#D19A66")
         (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))
         ;; maiblue-three-mono-1
         (face-remap-add-relative 'font-lock-variable-name-face :foreground "#ABB2BF"))
        )))

(add-hook 'after-change-major-mode-hook 'maiblue-three-theme-change-faces-for-mode)

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'maiblue-three-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; maiblue-three-theme.el ends here
