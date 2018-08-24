;;;
;;; MaiMacs 2017 - 2018, by MaiHD
;;;

(require 'cc-mode)

(setq c-default-style "linux")
(setq c++-default-style "linux")
(setq c-sharp-default-style "linux")

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq-default c-basic-offset 4)
(setq-default c-brace-imaginary-offset 4)
(setq-default c-continued-statement-offset 4)

(font-lock-add-keywords 'c++-mode
                        '(("uint" . 'font-lock-type-face)
                          ("int2" . 'font-lock-type-face)
                          ("int3" . 'font-lock-type-face)
                          ("int4" . 'font-lock-type-face)
                          ("uint2" . 'font-lock-type-face)
                          ("uint3" . 'font-lock-type-face)
                          ("uint4" . 'font-lock-type-face)
                          ("bool2" . 'font-lock-type-face)
                          ("bool3" . 'font-lock-type-face)
                          ("bool4" . 'font-lock-type-face)
                          ("float2" . 'font-lock-type-face)
                          ("float3" . 'font-lock-type-face)
                          ("float4" . 'font-lock-type-face)
                          ("int2x2" . 'font-lock-type-face)
                          ("int3x3" . 'font-lock-type-face)
                          ("int4x4" . 'font-lock-type-face)
                          ("uint2x2" . 'font-lock-type-face)
                          ("uint3x3" . 'font-lock-type-face)
                          ("uint4x4" . 'font-lock-type-face)
                          ("bool2x2" . 'font-lock-type-face)
                          ("bool3x3" . 'font-lock-type-face)
                          ("bool4x4" . 'font-lock-type-face)
                          ("float2x2" . 'font-lock-type-face)
                          ("float3x3" . 'font-lock-type-face)
                          ("float4x4" . 'font-lock-type-face)))
