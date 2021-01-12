/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

    enum OptKind[a] {
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


    pub def getOpt(ordering: ArgOrder[a], 
                   optDescrs: List[OptDescr[a]], 
                   sourceArgs: List[String]): (List[a], List[String], List[String]) =
        let (os,xs,us,es) = getOpt1(ordering, optDescrs, sourceArgs);
        (os, xs, es ::: List.map(errUnrec, us))

    pub def getOpt1(ordering: ArgOrder[a], 
                    optDescrs: List[OptDescr[a]], 
                    sourceArgs: List[String]): (List[a], List[String], List[String] , List[String]) = match sourceArgs {
        case Nil => (Nil, Nil, Nil, Nil)
        case arg :: args => {
            let (opt, rest)         = getNext(arg, optDescrs, args);
            let (os, xs, us, es)    = getOpt1(ordering, optDescrs, rest);
            match (opt, ordering) {
                case (Opt(o),       _)                  => (o :: os, xs, us, es)
                case (UnreqOpt(u),  _)                  => (os, xs, u :: us, es)
                case (NonOpt(x),    RequireOrder)       => (Nil,x :: rest, Nil, Nil)
                case (NonOpt(x),    Permute)            => (os, x :: xs, us, es)
                case (NonOpt(x),    ReturnInOrder(f))   => (f(x) :: os, xs, us, es)
                case (EndOfOpts,    RequireOrder)       => (Nil, rest, Nil, Nil)
                case (EndOfOpts,    Permute)            => (Nil, rest, Nil, Nil)
                case (EndOfOpts,    ReturnInOrder(f))   => (List.map(f, rest), Nil, Nil, Nil)
                case (OptErr(e),    _)                  => (os, xs, us, e :: es)
            }                
        }
    } 

    def getNext(arg: String, optDescrs: List[OptDescr[a]], tokens: List[String]): (OptKind[a], List[String]) = match arg {
        case "--"                                                       => (EndOfOpts, tokens)
        case s if String.startsWith(s, "--")                            => longOpt(String.dropLeft(2, s), optDescrs, tokens)
        case s if String.startsWith(s, "-") and String.length(s) >= 2   => shortOpt(String.charAt(1, s), String.dropLeft(2, s), optDescrs, tokens)
        case s                                                          => (NonOpt(s), tokens)
    }

    /// "--" has been parsed
    /// `xstr` is the rest of the current token
    def longOpt(xstr: String, optDescrs: List[OptDescr[a]], tokens: List[String]): (OptKind[a], List[String]) = 
        let (optname, arg)  = String.breakOnLeft("=", xstr);
        let getWith     = p -> List.filterMap(o -> { let xs = o.longOptions; if (List.exists(p(optname), xs)) Some(o) else None}, optDescrs);
        let exact       = getWith((x,y) -> x == y);
        let options     = if (List.isEmpty(exact)) getWith((sub, s1) -> String.startsWith(s1, sub)) else exact;
        let ads         = List.map(o -> o.argDescriptor, options);
        let optStr      = "--${optname}";
        match (ads, viewLeft(arg), tokens) {
            case (_ :: _ :: _,          _,                  rest)       => (errAmbig(options, optStr), rest)
            case (NoArg(a) :: Nil,      None,               rest)       => (Opt(a), rest)
            case (NoArg(_) :: Nil,      Some(('=', _)),     rest)       => (errNoArg(optStr), rest)
            case (ReqArg(_, d) :: Nil,  None,               Nil)        => (errReq(d, optStr), Nil)
            case (ReqArg(f, _) :: Nil,  None,               r :: rest)  => (Opt(f(r)), rest)
            case (ReqArg(f, _) :: Nil,  Some(('=', xs)),    rest)       => (Opt(f(xs)), rest)
            case (OptArg(f, _) :: Nil,  None,               rest)       => (Opt(f(None)), rest)
            case (OptArg(f, _) :: Nil,  Some(('=', xs)),    rest)       => (Opt(f(Some(xs))), rest)
            case (_,                    _,                  rest)       => (UnreqOpt("--${xstr}"), rest)
        }

    def viewLeft(s: String): Option[(Char, String)] = 
        if (String.length(s) >= 0)
            Some((String.charAt(0, s), String.dropLeft(1, s)))
        else
            None

    /// "-" has been parsed (i.e. x0).
    /// `xc` is the first char of the current token after the dash, which should identify the option.
    /// `xstr` is the rest of the current token
    /// Never see "=" for a short arg
    def shortOpt(xc: Char, xstr: String, optDescrs: List[OptDescr[a]], tokens: List[String]): (OptKind[a], List[String]) = 
        /// This finds 0, 1, or more results. "more" is a failure (ambiguous), 0 is ignored, 1 is success. 
        let options = List.filterMap(o -> { let ss = o.shortOptions; if (List.exists(s -> s == xc, ss)) Some(o) else None}, optDescrs);
        let ads     = List.map(o -> o.argDescriptor, options);
        let optStr  = "-${xc}";
        match (ads, xstr, tokens) {
            case (_ :: _ :: _, _, rest)                 => (errAmbig(options, optStr), rest)  /// TODO use Option instead if list?
            case (NoArg(a) :: _, "", rest)              => (Opt(a), rest)
            case (NoArg(a) :: _, str, rest)             => (Opt(a), "-${str}" :: rest)
            case (ReqArg(_, d) :: _, "", Nil)           => (errReq(d, optStr), Nil)
            case (ReqArg(f, _) :: _, "", r :: rest)     => (Opt(f(r)), rest)
            case (ReqArg(f, _) :: _, str, rest)         => (Opt(f(str)), rest)
            case (OptArg(f, _) :: _, "", rest)          => (Opt(f(None)),rest)
            case (OptArg(f, _) :: _, str, rest)         => (Opt(f(Some(str))),rest)
            case (Nil, "", rest)                        => (UnreqOpt(optStr), rest)
            case (Nil, str, rest)                       => (UnreqOpt(optStr), "-${str}" :: rest)
        }


    /// Error formatting functions

    def errAmbig(ods: List[OptDescr[a]], optStr: String): OptKind[a] = 
        let header = "option '${optStr}' is ambiguous; could be one of:";
        OptErr(usageInfo(header, ods))

    /// Requires an argument but it was missing.
    def errReq(d: String, optStr: String): OptKind[a] = 
        OptErr ("option '${optStr}' requires an argument ${d}\n")

    def errUnrec(optStr: String): String = "unrecognized option '${optStr}'\n"

    def errNoArg(optStr: String): OptKind[a] = 
        OptErr("option '${optStr}' doesn't allow an argument\n")

    /// Additions to List...

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

    pub def cartesianProduct(xs: List[a], ys: List[b]): List[(a,b)] = 
        let step = x -> List.map(y -> (x,y), ys);
        List.flatMap(step, xs)

}    