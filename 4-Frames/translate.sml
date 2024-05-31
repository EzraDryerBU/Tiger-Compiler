structure F = X86Frame

structure Translate : TRANSLATE =
struct
    datatype level = Outermost | Level of {prev_level : level, frame : X86Frame.frame}
    type access = level * X86Frame.access

    val outermost = Outermost
    fun newLevel {parent, name, formals} = (
      let
        val nf = F.newFrame {name=name, formals=true::formals}
      in
        Level({prev_level=parent, frame=nf})
      end

    )
    fun formals lev = (
      case lev of
        Outermost => ([])
        | Level({prev_level, frame}) => (
          let
            val forms = F.formals frame
            fun mapHelper fAccess = (
              (lev, fAccess)
            )
          in
            map mapHelper forms
          end
        ) 
    )
    fun allocLocal lev b = (
      case lev of
        Outermost => (lev, F.allocLocal (F.newFrame{name=Temp.newlabel(), formals=[]}) b)
        | Level({prev_level, frame}) => (
           (lev, F.allocLocal frame b)
      )
    )

    fun printAccess_ (_, acc) = X86Frame.printAccess acc
    fun printAccess n loc = print("var " ^ Symbol.name n ^ " " ^ printAccess_ loc ^ "\n")
    fun printLevel n l =
      print("function " ^ Symbol.name n ^ " "
            ^ (String.concatWith " " (map printAccess_ (formals l))) ^ "\n")end