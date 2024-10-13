(require 'cl-lib)

;;;; orgファイル中で引用されている文献のみを抽出したbibtexファイルを作成し、
;;;; orgファイルと同じディレクトリにreferences.bibとして出力する関数群
(defun --my-citar-get-key (entry)
  "Find the entry in ALIST where the key is '=type=' (as a string) and return its value."
  (let ((type-entry (assoc-string "=key=" entry)))  ;; 'assoc-string'を使って文字列キーを検索
    (if type-entry
        (cdr type-entry)  ;; エントリが見つかれば、その値を返す
      nil)))  ;; 見つからなければnilを返す

(defun --my-citar-get-type (entry)
  "Find the entry in ALIST where the key is '=type=' (as a string) and return its value."
  (let ((type-entry (assoc-string "=type=" entry)))  ;; 'assoc-string'を使って文字列キーを検索
    (if type-entry
        (cdr type-entry)  ;; エントリが見つかれば、その値を返す
      nil)))  ;; 見つからなければnilを返す

(defun --my-citar-get-fields (entry)
  "Remove entries from ALIST where the key contains SUBSTRING."
  (remove-if (lambda (pair)
               (string-match "=" (format "%s" (car pair))))  ;; キーにSUBSTRINGが含まれるかチェック
             entry))

(defun --my-citar-collect-citation ()
  "Collect cited entries from the current buffer using org-cite and citar."
  (let* ((citations
          (org-element-map (org-element-parse-buffer) 'citation
            (lambda (citation)
              (mapcar (lambda (ref)
                        (org-element-property :key ref))
                      (org-cite-get-references citation)))))
         (keys (apply #'append citations)))
    (message "Collected keys: %s" keys)  ;; Debugging output
    keys))  ;; Return the collected keys

(defun --my-citar-format-bibtex-fields (alist)
  "Format an association list ALIST into a string with 'キー = {値},' on each line."
  (mapconcat
   (lambda (pair)
     (format "%s = {%s}," (car pair) (cdr pair)))  ;; フォーマット: キー = {値},
   alist
   "\n"))  ;; 改行で各ペアを区切る

(defun --my-citar-format-bibtex-entry (citekey)
  "Reconstruct the BibTeX entry for CITEKEY from the citar-get-entry data."
  (let* ((entry (citar-get-entry citekey))  ;; BibTeXエントリーのalistを取得
         (entry-type (--my-citar-get-type entry))  ;; @type{ を取得
         (entry-key (--my-citar-get-key entry))  ;; citekeyを取得
         (fields (--my-citar-get-fields entry)))  ;; =type= と =key= を除外したフィールドを取得
    (concat
     (format "@%s{%s,\n" entry-type entry-key)  ;; @type{citekey, を生成
     (--my-citar-format-bibtex-fields (reverse fields))  ;; フィールドを逆順にしてフォーマット
     "\n}")))  ;; 閉じる

(defun my-citar-extract-citation (output-file)
  "Collect all cited BibTeX entries from the current Org buffer and export them to OUTPUT-FILE."
  (interactive "FSave to file: ")
  ;; 1. Collect all citekeys from the Org buffer
  (let ((citekeys (--my-citar-collect-citation)))  ;; Collect cited keys
    (with-current-buffer (get-buffer-create "*Collected BibTeX Entries*")
      (erase-buffer))  ;; Clear the buffer before starting

    ;; 2. For each citekey, find the corresponding BibTeX entry and copy it
    (dolist (key citekeys)
      (message "Processing citekey: %s" key)
      (condition-case nil
          (let ((bibtex-entry (--my-citar-format-bibtex-entry key)))  ;; Reconstruct the BibTeX entry
            (with-current-buffer "*Collected BibTeX Entries*"
              (goto-char (point-max))  ;; Move to the end of the buffer
              (insert bibtex-entry "\n\n")))  ;; Insert the BibTeX entry
        (error (message "Failed to process citekey: %s" key))))

    ;; 3. Write the collected BibTeX entries to the specified output file
    (with-current-buffer "*Collected BibTeX Entries*"
      (write-region (point-min) (point-max) output-file))
    (message "BibTeX entries saved to %s" output-file)))


;; (defun my-citar-export-hook (backend)
;;   "Hook function to collect citations and export to references.bib."
;;   (my-citar-extract-citation "references.bib"))  ;; 固定ファイル名でmy-citar-extract-citationを呼び出す

;; (add-hook 'org-export-before-processing-functions 'my-citar-export-hook)
