
use GetOpt.{ArgOrder};

def main(args: Array[String]): Int32 & Impure = 
    Array.foreach(Console.printLine, args);
    Console.printLine("Done");
    let (x,y) = String.breakOnLeft("=", "--output=hello.txt");
    Console.printLine("${x} / ${y}");
    match "" { 
        case "1"    => Console.printLine("one")
        case ""     => Console.printLine("[]")
        case _      => Console.printLine("_")
    };
    0