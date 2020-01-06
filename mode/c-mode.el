;;;
;;; MaiMacs 2017 - 2020, by MaiHD
;;;

(require 'cc-mode)

(setq c-default-style "linux")
(setq c++-default-style "linux")
(setq csharp-default-style "linux")

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq-default c-basic-offset 4)
(setq-default c-brace-imaginary-offset 4)
(setq-default c-continued-statement-offset 4)

(c-set-offset 'inline-open '0)
(c-set-offset 'substatement-open '0)

;;
;; Add HLSL types keywords highlight
;;
(let* ((types
        '("int2" "int3" "int4"
          "uint2" "uint3" "uint4" "uint"
          "bool2" "bool3" "bool4"
          "float2" "float3" "float4"
          "int2x2" "int3x3" "int4x4"
          "uint2x2" "uint3x3" "uint4x4"
          "bool2x2" "bool3x3" "bool4x4"
          "float2x2" "float3x3" "float4x4"))
       (types-regexp
        (concat "\\<\\(" (regexp-opt types) "\\)\\>")))
  (font-lock-add-keywords 'c++-mode `((,types-regexp . 'font-lock-type-face))))
