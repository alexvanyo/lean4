import Lean.Compiler.Kotlin

/-!
Tests `@[kotlin_expr]` and `@[extern kotlin "kotlin_expr:..."]` templates with `#i` placeholders:
calls are replaced by the template (also for small declarations that would otherwise be inlined),
and the Kotlin function of a `@[kotlin_expr]` declaration evaluates its template.
-/

namespace KotlinExprTemplate

@[kotlin_expr "(#1 * 2u)"] def dbl (x : UInt32) : UInt32 := x + x

@[extern kotlin "kotlin_expr:(#1 * 3u)"] def triple (x : UInt32) : UInt32 := x + x + x

@[kotlin_expr "(#1 - {1})"] def sub (x y : UInt32) : UInt32 := x - y

def useTemplates (x : UInt32) : UInt32 := dbl x + triple x + sub x 1

def mapDbl (xs : List UInt32) : List UInt32 := xs.map dbl

end KotlinExprTemplate
