use GetOpt.{ArgDescr, OptionDescr};
use GetOpt.ArgDescr.{NoArg, ReqArg, OptArg};
use GetOpt.ArgOrder.{RequireOrder, Permute, ReturnInOrder};
    
use GetOpt.{getOpt, usageInfo};

    
enum Flag {
    case Version
    case Help 
    case Verbose
    case Threads(Int32)
    case Listen(Int32)
}

instance ToString[Flag] {
    def toString(x: Flag): String = match x {
        case Version    => "Version"
        case Help       => "Help" 
        case Verbose    => "Verbose"
        case Threads(i) => "Threads(${i})"
        case Listen(i)  => "Listen(${i})"
    }
}

def options(): List[OptionDescr[Flag]] = 
       {optionIds = 'v' :: Nil,         optionNames = "version" :: Nil,     argDescriptor = NoArg(Version),                 explanation = "prints the version number"}
    :: {optionIds = 'h' :: '?' :: Nil,  optionNames = "help" :: Nil,        argDescriptor = NoArg(Help),                    explanation = "prints this usage information"}
    :: {optionIds = 'h' :: '?' :: Nil,  optionNames = "verbose" :: Nil,     argDescriptor = NoArg(Verbose),                 explanation = "enables verbose output"}
    :: {optionIds = 'o':: Nil,          optionNames = "threads" :: Nil,     argDescriptor = ReqArg(threads, "<threads>"),   explanation = "number of <threads> to use"}
    :: {optionIds = 'i':: Nil,          optionNames = "listen" :: Nil,      argDescriptor = ReqArg(listen,  "<port>"),      explanation = "starts the socket server and listens on <port>" }
    :: Nil

def threads(x: String): Option[Flag] = match Int32.fromString(x) { 
    case Ok(i) => Some(Threads(i))
    case Err(_) => None
}

def listen(x: String): Option[Flag] = match Int32.fromString(x) { 
    case Ok(i) => Some(Listen(i))
    case Err(_) => None
}



def main(args: Array[String]): Int32 & Impure = 
    let args2 = GetOpt.preprocess({quoteOpen = "'", quoteClose = "'", stripQuoteMarks = false}, args);  
    let header = "Usage: <program> [OPTION...] files...";
    let options1 = options();
    match getOpt(Permute, options1, args2) {
        case Success(opts)   => {Console.printLine("Optionals: ${opts.options}"); Console.printLine("Non-optionals: ${opts.nonOptions}"); 0}
        case Failure(errs)  => {Console.printLine("Errors: ${errs}");  Console.printLine(usageInfo(header, options1)); 1}
    }

