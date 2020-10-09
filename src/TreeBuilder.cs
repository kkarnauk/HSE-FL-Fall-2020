using System;

namespace RegexpParser
{
  internal class TreeBuilder
  {
    public static Node Build(string expression)
    {
      var builder = new TreeBuilder(expression);

      return builder.ReadNode();
    }
    

    Lexer myLexer;

    private TreeBuilder(string expression)
    {
      myLexer = new Lexer(expression);
    }

    private Node ReadNode()
    {
      if (myLexer.CurrentLexema == Lexema.EOF)
        return EmptyNode.Instance();
        
      if (myLexer.CurrentLexema == Lexema.OpenBracket)
      {
        myLexer.NextLexema();

        var result = ReadNode();

        if (myLexer.CurrentLexema != Lexema.CloseBracket)
          throw new ArgumentException("No close bracket.");

        myLexer.NextLexema(); // Reading close bracket

        return result;
      }

      if (myLexer.CurrentLexema == Lexema.String)
      {
        var currentString = myLexer.CurrentString;
        if (currentString == "")
          throw new ArgumentException("String cannot be empty.");

        Node currentNode = CharNode.Instance(currentString[0]);

        for (var i = 1; i < currentString.Length; i++) 
          currentNode = SeqNode.Instance(currentNode, CharNode.Instance(currentString[i]));

        myLexer.NextLexema();

        return currentNode;
      }

      var lexema = myLexer.CurrentLexema;

      myLexer.NextLexema();

      switch (lexema) 
      {
        case Lexema.Empty:
          return EmptyNode.Instance();
        case Lexema.Epsilon:
          return EpsilonNode.Instance();
        case Lexema.Seq:
          return SeqNode.Instance(ReadNode(), ReadNode());
        case Lexema.Alt:
          return AltNode.Instance(ReadNode(), ReadNode());
        case Lexema.Star:
          return StarNode.Instance(ReadNode());
        case Lexema.CloseBracket:
          throw new ArgumentException("No open bracket.");
        default:
          throw new InvalidOperationException("Something strange happened: unreachable code reached.");
      }
    }
  }
}