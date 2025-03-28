(defun extract-embedding-from-json (json-string)
  "Extract embedding vector from JSON response string."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'vector)
         (json-key-type 'string)
         (json (json-read-from-string json-string)))
    (gethash "embedding" json)))

(defun cosine-similarity (vec1 vec2)
  "Calculate cosine similarity between two vectors."
  (let ((dot-product 0)
        (norm1 0)
        (norm2 0))
    (dotimes (i (length vec1))
      (setq dot-product (+ dot-product (* (aref vec1 i) (aref vec2 i))))
      (setq norm1 (+ norm1 (* (aref vec1 i) (aref vec1 i))))
      (setq norm2 (+ norm2 (* (aref vec2 i) (aref vec2 i)))))
    (/ dot-product (* (sqrt norm1) (sqrt norm2)))))

(defun compare-embeddings (json1 json2)
  "Compare embeddings from two JSON response strings."
  (let ((emb1 (extract-embedding-from-json json1))
        (emb2 (extract-embedding-from-json json2)))
    (cosine-similarity emb1 emb2)))

(provide 'embedding-utils)
