# drakon
(Partial) C++ parser written in Haskell / Parsec

This is a partial C++ parser written with the versatile Parsec library. The lexer should be complete of course,
but the parser hit the typical C++ parsing wall where you first have to do semantic analysis to complete the
syntactic analysis, which would be fairly non-trivial to work around. I left it in this state because it is
a good example of how Parsec may be used to work in a real-world setting. If someone completes it, of course,
I would be grateful.

Dr. Eray Ozkural
