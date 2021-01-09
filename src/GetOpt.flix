

namespace GetOpt {


    pub enum ArgOrder[a] {
        case RequireOrder
        case Permute
        case ReturnInOrder(String -> a)
    }
    
    pub enum ArgDescr[a] {
        case NoArg(a)
        case ReqArg(String -> a, String)
        case OptArg(Option[String] -> a, String)
    }

    /// TODO not public
    pub enum OptKind[a] {
        case Opt(a)
        case UnreqOpt(String)
        case NonOpt(String)
        case EndOfOpts
        case OptErr(String)
    }

    type alias OptDescr[a] =
        { shortOptions: List[Char]
        , longOptions: List[String]
        , argDescriptor: ArgDescr[a]
        , explanation: String
        }


    pub def usageInfo(header: String, optDescriptors: List[OptDescr[a]]): String = 
        let paste = (x, y, z) -> "  ${x}  ${y}  ${z}";
        let build = List.flatMap(formatOpt) >> unzip3;
        let (shorts, longs, ds) = build(optDescriptors);
        let table = zipWith3(paste, sameLen(shorts), sameLen(longs), ds);
        String.unlines(header :: table)

    def sameLen(xs: List[String]): List[String] = 
        let width = List.foldLeft((ac, x) -> Int32.max(ac, String.length(x)), 0, xs);
        List.map(String.padRight(width, ' '), xs)

    def formatOpt(opts: OptDescr[a]): List[(String, String, String)] = 
        let shorts = List.map(formatShort(opts.argDescriptor),  opts.shortOptions) |> sepBy(',');
        let longs = List.map(formatLong(opts.argDescriptor), opts.longOptions) |> sepBy(',');
        match String.lines(opts.explanation) { 
            case Nil => (shorts, longs, "") :: Nil
            case d :: ds => (shorts, longs, d) :: List.map(x -> ("", "", x), ds)
        }

    def sepBy(ch: Char, ss: List[String]): String = 
        let sep = "${ch} ";
        String.intercalate(sep, ss)


    def formatShort(argDescr: ArgDescr[a], short: Char): String = match argDescr {
        case NoArg(_) => "-${short}"
        case ReqArg(_, descr) => "-${short} ${descr}"
        case OptArg(_, descr) => "-${short}[${descr}]"
    }

    def formatLong(argDescr: ArgDescr[a], long: String): String = match argDescr {
        case NoArg(_) => "--${long}"
        case ReqArg(_, descr) => "--${long}=${descr}"
        case OptArg(_, descr) => "--${long}[=${descr}]"
    }

    pub def unzip3(xs: List[(a, b, c)]): (List[a], List[b], List[c]) = unzip3Helper(xs, (ks1, ks2, ks3) -> (ks1, ks2, ks3))

    def unzip3Helper(xs: List[(a, b, c)], k: (List[a], List[b], List[c]) -> (List[a], List[b], List[c])): (List[a], List[b], List[c]) = match xs {
        case Nil => k(Nil, Nil, Nil)
        case (x1, x2, x3) :: rs => unzip3Helper(rs, (ks1, ks2, ks3) -> k(x1 :: ks1, x2 :: ks2, x3 :: ks3))
    }

    pub def zipWith3(f: (a, b, c) -> d & ef, xs: List[a], ys: List[b], zs: List[c]): List[d] & ef = 
        zipWith3Helper(f, xs, ys, zs, ks -> ks)

    def zipWith3Helper(f: (a, b, c) -> d & ef, xs: List[a], ys: List[b], zs: List[c], k: List[d] -> List[d]): List[d] & ef = 
        match (xs, ys, zs) {
            case (x :: rs, y :: qs, z :: ss) => { 
                let a = f(x, y, z);
                zipWith3Helper(f, rs, qs, ss, ks -> k(a :: ks))
            }
            case _ => k(Nil)
        }


}    