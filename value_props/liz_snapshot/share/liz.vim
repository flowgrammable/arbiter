" VIM syntax file
" Language: Liz
" Maintainer: C. Jasson Casey
" Latest Revision: November 2012

if exists("b:current_syntax")
   finish
endif

" Basic language keywords
syn keyword lizTypes bool byte char int double void const struct
syn keyword lizDepTypes bits list array union tag
syn keyword lizBasicKeywords declare define namespace typename template
syn keyword lizBasicKeywords rule concept requires using public private

" Contorl keywords
syn keyword lizControl switch case break if then else while repeat 
syn keyword lizControl do until for return throw leave goto

" Logical operator keywords
syn keyword lizOps or and not in

" Logic keywords
syn keyword lizLogic axiom assume forall exist

"# + - * / % | & ~ = ++ -- < <= >= == !=

"syn match lizSeparator '[\]\]{}():,;]'

syn region lizBlock start="{" end="}" fold transparent
syn region lizString start='"' end='"'

"- -> => <=> ( ) [ ] { } [| |] :: : . , ;

let b:current_syntax = "liz"

hi def link lizControl Constant
hi def link lizTypes Type
hi def link lizBasicKeywords Statement
hi def link lizOps Operator
hi def link lizLogic special
hi def link lizString comment
hi def link lizDepTypes PreProc
