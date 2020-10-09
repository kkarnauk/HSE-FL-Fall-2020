namespace RegexpParser
{
  internal abstract class Node 
  {
    public abstract Node Derivative(char symbol);
    
    public abstract bool Nullable { get; }
  }


  internal class EmptyNode : Node
  {
    public override Node Derivative(char symbol) => this;

    public override bool Nullable => false;

    private EmptyNode() {}

    private static EmptyNode myInstance = new EmptyNode();

    public static Node Instance() => myInstance;
  }


  internal class EpsilonNode : Node 
  {
    public override Node Derivative(char symbol) => EmptyNode.Instance();

    public override bool Nullable => true;

    private EpsilonNode() {}

    private static Node myInstance = new EpsilonNode();

    public static Node Instance() => myInstance;
  }


  internal class CharNode : Node 
  {
    private char myChar;

    public override Node Derivative(char symbol) 
    {
      if (symbol == myChar)
        return EpsilonNode.Instance();
      else 
        return EmptyNode.Instance();
    }

    public override bool Nullable => false;

    private CharNode(char symbol) 
    {
      myChar = symbol;
    }

    public static Node Instance(char symbol) => new CharNode(symbol);
  }


  internal class AltNode : Node 
  {
    private Node myLeftNode;
    private Node myRightNode;

    private bool myNullable;

    public override Node Derivative(char symbol) => 
      AltNode.Instance(myLeftNode.Derivative(symbol), myRightNode.Derivative(symbol));

    public override bool Nullable => myNullable;

    private AltNode(Node leftNode, Node rightNode) 
    {
      myLeftNode = leftNode;
      myRightNode = rightNode;

      myNullable = myLeftNode.Nullable || myRightNode.Nullable;
    }

    public static Node Instance(Node leftNode, Node rightNode)
    {
      if (leftNode is EmptyNode) 
        return rightNode;
      
      if (rightNode is EmptyNode)
        return leftNode;

      if (leftNode is EpsilonNode && rightNode.Nullable)
        return rightNode;

      if (rightNode is EpsilonNode && leftNode.Nullable)
        return leftNode;
      
      return new AltNode(leftNode, rightNode);
    }
  }


  internal class SeqNode : Node
  {
    private Node myLeftNode;
    private Node myRightNode;

    private bool myNullable;

    public override Node Derivative(char symbol)
    {
      if (myLeftNode.Nullable)
      {
        var left = SeqNode.Instance(myLeftNode.Derivative(symbol), myRightNode);
        var right = myRightNode.Derivative(symbol);

        return AltNode.Instance(left, right);
      }
      else
      {
        return SeqNode.Instance(myLeftNode.Derivative(symbol), myRightNode);
      }
    }

    public override bool Nullable => myNullable;

    private SeqNode(Node leftNode, Node rightNode)
    {
      myLeftNode = leftNode;
      myRightNode = rightNode;

      myNullable = myLeftNode.Nullable && myRightNode.Nullable;
    }

    public static Node Instance(Node leftNode, Node rightNode) 
    {
      if (leftNode is EmptyNode || rightNode is EmptyNode)
        return EmptyNode.Instance();
      
      if (leftNode is EpsilonNode)
        return rightNode;
      
      if (rightNode is EpsilonNode)
        return leftNode;

      return new SeqNode(leftNode, rightNode);
    }
  }
  

  internal class StarNode : Node 
  {
    Node myChildNode;

    public override Node Derivative(char symbol) =>
      SeqNode.Instance(myChildNode.Derivative(symbol), StarNode.Instance(myChildNode));

    public override bool Nullable => true;

    private StarNode(Node node)
    {
      myChildNode = node;
    }

    public static Node Instance(Node node)
    {
      if (node is EmptyNode || node is EpsilonNode)
        return EpsilonNode.Instance();

      if (node is StarNode)
        return node;

      return new StarNode(node);
    }
  }
}