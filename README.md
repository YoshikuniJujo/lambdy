lambdy
======

何を作るか
----------

Haskellのサブセットとしてのラムダ式を、実行形式にコンパイルする。
出力はLLVM IRとして、それをllcでコンパイルする。
そういう何かを作る。

何のために
----------

LLVMの勉強のため。

何で作るか
----------

Haskell。パーサにはPEGのPackratパーサであるpapillonを使う。
papillonは無駄に多機能だが、最小限の機能だけ使う。
