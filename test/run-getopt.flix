
use GetOpt.{ArgOrder};

def main(args: Array[String]): Int32 & Impure = 
    Array.foreach(Console.printLine, args);
    Console.printLine("Done");
    0