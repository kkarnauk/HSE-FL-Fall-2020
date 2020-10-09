using System.IO;

namespace SyntacticalAnalyzer
{
  internal class Parser
  {
    public static ParserResult Parse(string relation)
    {
      var parser = new Parser(relation);

      while (parser.myLexer.CurrentLexema != Lexema.EOF)
      {
        if (!parser.ParseRelation())
          return new ParserResult(false, parser.myLexer.CurrentLine, parser.myLexer.CurrentColon);
      }

      return new ParserResult(true);
    }


    private Lexer myLexer;

    private Parser(string relation)
    {
      myLexer = new Lexer(new StringReader(relation));
    }

    private bool ParseRelation()
    {
      if (!ParseLexema(Lexema.Identifier))
        return false;

      if (ParseLexema(Lexema.Dot))
        return true;

      if (!ParseLexema(Lexema.Turnstile))
        return false;

      if (!ParseDisjunction())
        return false;

      return ParseLexema(Lexema.Dot);
    }

    private bool ParseDisjunction()
    {
      if (!ParseConjuction())
        return false;

      while (ParseLexema(Lexema.Semicolon))
      { 
        if (!ParseConjuction())
          return false;
      }

      return true;
    }

    private bool ParseConjuction()
    {
      if (!ParseLeaf())
        return false;
      
      while (ParseLexema(Lexema.Comma))
      { 
        if (!ParseLeaf())
          return false;
      }

      return true;
    }

    private bool ParseLeaf()
    {
      if (ParseLexema(Lexema.OpenBracket))
      {
        if (!ParseDisjunction() || !ParseLexema(Lexema.CloseBracket))
          return false;
      }
      else if (!ParseLexema(Lexema.Identifier)) {
        return false;
      }

      return true;
    }

    private bool ParseLexema(Lexema lexema)
    {
      if (myLexer.CurrentLexema != lexema)
        return false;

      myLexer.NextLexema();

      return true;
    }

    internal struct ParserResult
    {
      public bool Success { get; }

      public int ErrorLine { get; }
      public int ErrorColon { get; }

      public ParserResult(bool success, int errorLine = -1, int errorColon = -1)
      {
        Success = success;
        ErrorLine = errorLine;
        ErrorColon = errorColon;
      }
    }
  }
}