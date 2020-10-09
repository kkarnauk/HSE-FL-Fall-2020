namespace RegexpParser
{
  internal enum Lexema 
  {
    Empty,
    Epsilon,
    String,
    Seq,
    Alt,
    Star,
    OpenBracket,
    CloseBracket,
    EOF
  }
}