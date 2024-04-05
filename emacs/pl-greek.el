(provide 'pl-greek)

(require 'quail)

(quail-define-package
 "pl-greek" "PlGreek" "Programming languages greek mode thingy" t
 "pl-greek"
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("\\Alpha" "Α") ("\\alpha" "α") ("\\Beta" "Β") ("\\beta" "β")
 ("\\Gamma" "Γ") ("\\gamma" "γ") ("\\Delta" "Δ") ("\\delta" "δ")
 ("\\Epsilon" "Ε") ("\\epsilon" "ε") ("\\Zeta" "Ζ") ("\\zeta" "ζ")
 ("\\Eta" "Η") ("\\eta" "η") ("\\Theta" "Θ") ("\\theta" "θ")
 ("\\Iota" "Ι") ("\\iota" "ι") ("\\Kappa" "Κ") ("\\kappa" "κ")
 ("\\Lambda" "Λ") ("\\lambda" "λ") ("\\Mu" "Μ") ("\\mu" "μ")
 ("\\Nu" "Ν") ("\\nu" "ν") ("\\Xi" "Ξ") ("\\xi" "ξ")
 ("\\Omicron" "Ο") ("\\omicron" "ο") ("\\Pi" "Π") ("\\pi" "π")
 ("\\Rho" "Ρ") ("\\rho" "ρ") ("\\varrho" "ϱ") ("\\Sigma" "Σ") ("\\sigma" "σ")
 ("\\Tau" "Τ") ("\\tau" "τ") ("\\Upsilon" "Υ") ("\\upsilon" "υ")
 ("\\Phi" "Φ") ("\\phi" "φ") ("\\Chi" "Χ") ("\\chi" "χ")
 ("\\Psi" "Ψ") ("\\psi" "ψ") ("\\Omega" "Ω") ("\\omega" "ω")
 ;; 
 ("\\arrow" "→") ("\\implies" "⇒")
 ("\\mapsto" "↦")
 ("\\partial" "⇀")

 ("\\natural" "♮")
 ("\\dot" "·")
 ("\\union" "⋃")
 ("\\intersect" "⋂")
 ("\\subset" "⊂") ("\\subseteq" "⊆") ("\\subsetneq" "⊊")
 ("\\in" "∈") ("\\nin" "∉")
 ("\\ell" "ℓ")

 ("\\hookright" "↪")

 ("\\not" "¬")
 ("\\ne" "≠")

 ("\\ts" "⊢")
 ("\\epsilon" ?ϵ)
 ("\\phi" ?ϕ)
 ("\\Box" ?□)
 ("\\Bumpeq" ?≎)
 ("\\Cap" ?⋒)
 ("\\Cup" ?⋓)
 ("\\Diamond" ?◇)
 ("\\Downarrow" ?⇓)
 ("\\H{o}" ?ő)
 ("\\Im" ?ℑ)
 ("\\Join" ?⋈)
 ("\\Leftarrow" ?⇐)
 ("\\Leftrightarrow" ?⇔)
 ("\\Ll" ?⋘)
 ("\\Lleftarrow" ?⇚)
 ("\\Longleftarrow" ?⇐)
 ("\\Longleftrightarrow" ?⇔)
 ("\\Longrightarrow" ?⇒)
 ("\\Lsh" ?↰)
 ("\\Re" ?ℜ)
 ("\\Rightarrow" ?⇒)
 ("\\Rrightarrow" ?⇛)
 ("\\Rsh" ?↱)
 ("\\Subset" ?⋐)
 ("\\Supset" ?⋑)
 ("\\Uparrow" ?⇑)
 ("\\Updownarrow" ?⇕)
 ("\\Vdash" ?⊩)
 ("\\Vert" ?‖)
 ("\\Vvdash" ?⊪)
 ("\\above" ?┴)
 ("\\aleph" ?ℵ)
 ("\\amalg" ?∐)
 ("\\angle" ?∠)
 ("\\aoint" ?∳)
 ("\\approx" ?≈)
 ("\\approxeq" ?≊)
 ("\\asmash" ?⬆)
 ("\\ast" ?∗)
 ("\\asymp" ?≍)
 ("\\atop" ?¦)
 ("\\backcong" ?≌)
 ("\\backepsilon" ?∍)
 ("\\backprime" ?‵)
 ("\\backsim" ?∽)
 ("\\backsimeq" ?⋍)
 ("\\barwedge" ?⊼)
 ("\\because" ?∵)
 ("\\begin" ?\〖)
 ("\\below" ?┬)
 ("\\beth" ?ℶ)
 ("\\between" ?≬)
 ("\\bigcap" ?⋂)
 ("\\bigcirc" ?◯)
 ("\\bigcup" ?⋃)
 ("\\bigodot" ?⨀)
 ("\\bigoplus" ?⨁)
 ("\\bigotimes" ?⨂)
 ("\\bigsqcup" ?⨆)
 ("\\biguplus" ?⨄)
 ("\\bigstar" ?★)
 ("\\bigtriangledown" ?▽)
 ("\\bigtriangleup" ?△)
 ("\\bigvee" ?⋁)
 ("\\bigwedge" ?⋀)
 ("\\blacklozenge" ?✦)
 ("\\blacksquare" ?▪)
 ("\\blacktriangle" ?▴)
 ("\\blacktriangledown" ?▾)
 ("\\blacktriangleleft" ?◂)
 ("\\blacktriangleright" ?▸)
 ("\\bot" ?⊥)
 ("\\bowtie" ?⋈)
 ("\\boxminus" ?⊟)
 ("\\boxplus" ?⊞)
 ("\\boxtimes" ?⊠)
 ("\\bra" ?\⟨)
 ("\\bullet" ?•)
 ("\\bumpeq" ?≏)
 ("\\cap" ?∩)
 ("\\cdots" ?⋯)
 ("\\centerdot" ?·)
 ("\\checkmark" ?✓)
 ("\\chi" ?χ)
 ("\\circ" ?∘)
 ("\\circeq" ?≗)
 ("\\circlearrowleft" ?↺)
 ("\\circlearrowright" ?↻)
 ("\\circledR" ?®)
 ("\\coint" ?∲)
 ("\\coloneq" ?≔)
 ("\\complement" ?∁)
 ("\\cong" ?≅)
 ("\\coprod" ?∐)
 ("\\cup" ?∪)
 ("\\curlyeqprec" ?⋞)
 ("\\curlyeqsucc" ?⋟)
 ("\\curlypreceq" ?≼)
 ("\\curlyvee" ?⋎)
 ("\\curlywedge" ?⋏)
 ("\\curvearrowleft" ?↶)
 ("\\curvearrowright" ?↷)

 ("\\dag" ?†)
 ("\\dagger" ?†)
 ("\\daleth" ?ℸ)
 ("\\dashv" ?⊣)
 ("\\Dd" ?ⅅ)
 ("\\dd" ?ⅆ)
 ("\\ddag" ?‡)
 ("\\ddagger" ?‡)
 ("\\ddddot" ?⃜)
 ("\\dddot" ?⃛)
 ("\\ddots" ?⋱)
 ("\\diamond" ?⋄)
 ("\\diamondsuit" ?♢)
 ("\\divideontimes" ?⋇)
 ("\\doteq" ?≐)
 ("\\doteqdot" ?≑)
 ("\\dotplus" ?∔)
 ("\\dotsquare" ?⊡)
 ("\\downarrow" ?↓)
 ("\\downdownarrows" ?⇊)
 ("\\downleftharpoon" ?⇃)
 ("\\downrightharpoon" ?⇂)
 ("\\dsmash" ?⬇)
 ("\\ee" ?ⅇ)
 ("\\ell" ?ℓ)
 ("\\emptyset" ?∅)
 ("\\eqarray" ?█)
 ("\\eqcirc" ?≖)
 ("\\eqcolon" ?≕)
 ("\\eqslantgtr" ?⋝)
 ("\\eqslantless" ?⋜)
 ("\\equiv" ?≡)
 ("\\exists" ?∃)
 ("\\fallingdotseq" ?≒)
 ("\\flat" ?♭)
 ("\\forall" ?∀)
 ("\\frac1" ?⅟)
 ("\\frac12" ?½)
 ("\\frac13" ?⅓)
 ("\\frac14" ?¼)
 ("\\frac15" ?⅕)
 ("\\frac16" ?⅙)
 ("\\frac18" ?⅛)
 ("\\frac23" ?⅔)
 ("\\frac25" ?⅖)
 ("\\frac34" ?¾)
 ("\\frac35" ?⅗)
 ("\\frac38" ?⅜)
 ("\\frac45" ?⅘)
 ("\\frac56" ?⅚)
 ("\\frac58" ?⅝)
 ("\\frac78" ?⅞)
 ("\\frown" ?⌢)
 ("\\ge" ?≥)
 ("\\geq" ?≥)
 ("\\geqq" ?≧)
 ("\\geqslant" ?≥)
 ("\\gets" ?←)
 ("\\gg" ?≫)
 ("\\ggg" ?⋙)
 ("\\gimel" ?ℷ)
 ("\\gnapprox" ?⋧)
 ("\\gneq" ?≩)
 ("\\gneqq" ?≩)
 ("\\gnsim" ?⋧)
 ("\\gtrapprox" ?≳)
 ("\\gtrdot" ?⋗)
 ("\\gtreqless" ?⋛)
 ("\\gtreqqless" ?⋛)
 ("\\gtrless" ?≷)
 ("\\gtrsim" ?≳)
 ("\\gvertneqq" ?≩)
 ("\\hbar" ?ℏ)
 ("\\heartsuit" ?♥)
 ("\\hookleftarrow" ?↩)
 ("\\hookrightarrow" ?↪)
 ("\\hphantom" ?⬄)
 ("\\hsmash" ?⬌)
 ("\\iff" ?⇔)
 ("\\ii" ?ⅈ)
 ("\\looparrowleft" ?↫)
 ("\\looparrowright" ?↬)
 ("\\lozenge" ?✧)
 ("\\lrcorner" ?⌟)
 ("\\ltimes" ?⋉)
 ("\\lvertneqq" ?≨)
 ("\\maltese" ?✠)
 ("\\mapsto" ?↦)
 ("\\measuredangle" ?∡)
 ("\\mho" ?℧)
 ("\\mid" ?∣)
 ("\\models" ?⊧)
 ("\\mp" ?∓)
 ("\\multimap" ?⊸)
 ("\\nLeftarrow" ?⇍)
 ("\\nLeftrightarrow" ?⇎)
 ("\\nRightarrow" ?⇏)
 ("\\nVDash" ?⊯)
 ("\\nVdash" ?⊮)
 ("\\nabla" ?∇)
 ("\\napprox" ?≉)
 ("\\natural" ?♮)
 ("\\ncong" ?≇)
 ("\\ne" ?≠)
 ("\\nearrow" ?↗)
 ("\\neg" ?¬)
 ("\\neq" ?≠)
 ("\\nequiv" ?≢)
 ("\\nexists" ?∄)
 ("\\ngeq" ?≱)
 ("\\ngeqq" ?≱)
 ("\\ngeqslant" ?≱)
 ("\\ngtr" ?≯)
 ("\\ni" ?∋)
 ("\\nleftarrow" ?↚)
 ("\\nleftrightarrow" ?↮)
 ("\\nleq" ?≰)
 ("\\nleqq" ?≰)
 ("\\nleqslant" ?≰)
 ("\\nless" ?≮)
 ("\\nmid" ?∤)
 ("\\nparallel" ?∦)
 ("\\nprec" ?⊀)
 ("\\npreceq" ?⋠)
 ("\\nrightarrow" ?↛)
 ("\\nshortmid" ?∤)
 ("\\nshortparallel" ?∦)
 ("\\nsim" ?≁)
 ("\\nsimeq" ?≄)
 ("\\nsubset" ?⊄)
 ("\\nsubseteq" ?⊈)
 ("\\nsubseteqq" ?⊈)
 ("\\nsucc" ?⊁)
 ("\\nsucceq" ?⋡)
 ("\\nsupset" ?⊅)
 ("\\nsupseteq" ?⊉)
 ("\\nsupseteqq" ?⊉)
 ("\\ntriangleleft" ?⋪)
 ("\\ntrianglelefteq" ?⋬)
 ("\\ntriangleright" ?⋫)
 ("\\ntrianglerighteq" ?⋭)
 ("\\nvDash" ?⊭)
 ("\\nvdash" ?⊬)
 ("\\nwarrow" ?↖)
 ("\\odot" ?⊙)
 ("\\oint" ?∮)
 ("\\ominus" ?⊖)
 ("\\oplus" ?⊕)
 ("\\oslash" ?⊘)
 ("\\otimes" ?⊗)
 ("\\overbrace" ?⏞)
 ("\\overparen" ?⏜)
 ("\\parallel" ?∥)
 ("\\partial" ?∂)
 ("\\perp" ?⊥)
 ("\\phantom" ?⟡)
 ("\\pitchfork" ?⋔)
 ("\\pppprime" ?⁗)
 ("\\ppprime" ?‴)
 ("\\pprime" ?″)
 ("\\prcue" ?≼)
 ("\\prec" ?≺)
 ("\\precapprox" ?≾)
 ("\\preceq" ?≼)
 ("\\precnapprox" ?⋨)
 ("\\precnsim" ?⋨)
 ("\\precsim" ?≾)
 ("\\prime" ?′)
 ("\\prod" ?∏)
 ("\\propto" ?∝)
 ("\\qdrt" ?∜)
 ("\\qed" ?∎)
 ("\\ratio" ?∶)
 ("\\rceil" ?⌉)
 ("\\rddots" ?⋰)
 ("\\rect" ?▭)
 ("\\rfloor" ?⌋)
 ("\\rightarrow" ?→)
 ("\\rightarrowtail" ?↣)
 ("\\rightharpoondown" ?⇁)
 ("\\rightharpoonup" ?⇀)
 ("\\rightleftarrows" ?⇄)
 ("\\rightleftharpoons" ?⇌)
 ("\\rightrightarrows" ?⇉)
 ("\\rightthreetimes" ?⋌)
 ("\\risingdotseq" ?≓)
 ("\\rrect" ?▢)
 ("\\rtimes" ?⋊)
 ("\\searrow" ?↘)
 ("\\setminus" ?∖)
 ("\\sharp" ?♯)
 ("\\shortmid" ?∣)
 ("\\shortparallel" ?∥)
 ("\\sim" ?∼)
 ("\\simeq" ?≃)
 ("\\smallamalg" ?∐)
 ("\\smallsetminus" ?∖)
 ("\\smallsmile" ?⌣)
 ("\\smash" ?⬍)
 ("\\smile" ?⌣)
 ("\\spadesuit" ?♠)
 ("\\sphericalangle" ?∢)
 ("\\sqcap" ?⊓)
 ("\\sqcup" ?⊔)
 ("\\sqsubset" ?⊏)
 ("\\sqsubseteq" ?⊑)
 ("\\sqsupset" ?⊐)
 ("\\sqsupseteq" ?⊒)
 ("\\square" ?□)
 ("\\squigarrowright" ?⇝)
 ("\\subset" ?⊂)
 ("\\subseteq" ?⊆)
 ("\\subseteqq" ?⊆)
 ("\\subsetneq" ?⊊)
 ("\\subsetneqq" ?⊊)
 ("\\succ" ?≻)
 ("\\succapprox" ?≿)
 ("\\succcurlyeq" ?≽)
 ("\\succeq" ?≽)
 ("\\succnapprox" ?⋩)
 ("\\succnsim" ?⋩)
 ("\\succsim" ?≿)
 ("\\sum" ?∑)
 ("\\supset" ?⊃)
 ("\\supseteq" ?⊇)
 ("\\supseteqq" ?⊇)
 ("\\supsetneq" ?⊋)
 ("\\supsetneqq" ?⊋)
 ("\\to" ?→)
 ("\\top" ?⊤))


