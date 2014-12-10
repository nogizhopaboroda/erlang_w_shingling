erlang_w_shingling
==================

W-shingling algorithm realization in erlang.

##### DESCRIPTION:
  This module contains functions to compare similarity in texts with W-shingling algorithm.
  
  Information about algorithm: http://en.wikipedia.org/wiki/W-shingling

##### EXPORTS:
  ```erlang
  similarity(Text1, Text2, ShingleLength) -> float()
  ```
  Returns a similarity of two texts with defined shingle length in percents.
  
  ```erlang
  similarity(Text1, Text2) -> float()
  ```
  Same as previous, but with default shingle length.
  


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/nogizhopaboroda/erlang_w_shingling/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

