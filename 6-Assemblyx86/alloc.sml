structure Alloc = struct

structure A = Assem
structure F = X86Frame

structure TT = Temp.Table

type interval = Live.interval

datatype alloced = Reg of F.register * (int option) | Spilled of int

val eax = (X86Frame.RV, "%eax")
val ecx = (Temp.newtemp (), "%ecx")
val edx = (Temp.newtemp (), "%edx")
val ebx = (Temp.newtemp (), "%ebx")
val edi = (Temp.newtemp (), "%edi")
val esi = (Temp.newtemp (), "%esi")

val args = [ebx, edi, esi, ecx]

fun spillLoc (Reg (_, SOME sp)) = sp
  | spillLoc (Spilled sp) = sp
  | spillLoc _ = ErrorMsg.impossible "spillLoc called on Register with no spill location"

fun spillLocOpt frame (SOME a) = spillLoc a
  | spillLocOpt frame NONE = F.spillLoc frame 

val initArgs = map (fn (_, x) => x) args

val initMap = 
    let
        fun add ((t, s), table) = Temp.Table.enter(table, t, s)
    in
        foldr add Temp.Table.empty ([(F.FP, Reg ("%ebp", NONE)), (F.RV, Reg ("%eax", NONE))])
    end

fun insertBy (f : 'a * 'a -> bool) (x : 'a) ([] : 'a list) = [x]
  | insertBy f x (y :: ys) =
    if f (x, y) then x :: y :: ys else y :: insertBy f x ys

fun insertByFinish (i : interval) (ins : interval list) =
    insertBy (fn(t1, t2) => #finish t1 < #finish t2) i ins

fun allocRecords (frame : X86Frame.frame) (ins : interval list) (assem : (Temp.temp A.instr) list) : (F.register A.instr) list =
    let
    val ilist = ref (nil : (F.register A.instr) list)
    fun emit x = ilist := x :: !ilist

    fun relTemps (A.OPER { src, dst, ...}) = src @ dst
      | relTemps (A.LABEL _) = []
      | relTemps (A.JUMP _) = []

    fun spillAll aenv =
        IntBinaryMap.map (fn a => case a
                                    of Reg (r, SOME i) =>
                                        (emit (A.OPER { assem = "MOV `s0, -" ^ Int.toString (~i) ^ "(`d0)\n", dst = ["%ebp"], src = [r] });
                                        Spilled i)
                                     | Reg r => Reg r
                                     | Spilled i => Spilled i) aenv

    fun repTemp (aenv : alloced TT.table) (t : Temp.temp) =
        case TT.look(aenv, t)
            of SOME (Reg (r, _)) => r
            | SOME (Spilled i) => ErrorMsg.impossible ("register spilled: " ^ Temp.makestring t)
            | NONE => ErrorMsg.impossible ("register not found: " ^ Temp.makestring t)

    and substReg (aenv : alloced TT.table)
                 us al
                 (oper as A.OPER {assem, dst, src } : Temp.temp A.instr) : (alloced TT.table * F.register list) =
            let
                val (aenv', us') = foldl (fn (t, (aenv, us)) =>
                                                let val (aenv', _, us') = getReg aenv us al t oper in (aenv', us') end) (aenv, us) (src @ dst)
            in
            emit(A.OPER { assem = assem, dst = map (repTemp aenv') dst, src = map (repTemp aenv') src });
            (aenv', us')
            end
    | substReg aenv us _ (A.LABEL {assem, lab}) = let val aenv' = spillAll aenv in emit (A.LABEL {assem = assem, lab = lab}); (aenv', initArgs) end
    | substReg aenv us _ (A.JUMP {assem, jump}) = let val aenv' = spillAll aenv in emit (A.JUMP {assem = assem, jump = jump}); (aenv', initArgs) end

    (* Find a register that is not used by the next instruction, and thus can be spilled*)
    and spillable aenv (al : interval list) instr =
        let
            val rel = relTemps instr
            fun notspilled a = case TT.look(aenv, a)
                                of SOME (Reg _) => true
                                 | _ => false
            val spill = List.find (fn {temp, ...} => List.all (fn r => r <> temp andalso notspilled temp) rel) al
        in
        spill
        end

    and adjustSpilled curr_mapping reg : unit =
        case curr_mapping
            of SOME (Spilled i) =>
                emit (A.OPER { assem = "MOV -" ^ Int.toString (~i) ^ "(`s0), `d0\n", dst = [reg], src = ["%ebp"] })
             | _ => ()

    and getReg (aenv : alloced TT.table) (us : F.register list) (al : interval list)
               (temp : Temp.temp) (instr : Temp.temp A.instr) : alloced TT.table * F.register * F.register list =
        let
            val curr_mapping = TT.look(aenv, temp)
        in
        case curr_mapping
            of SOME (Reg (r, _)) => (aenv, r, us)
             | _ =>
                (case us
                    of u :: us' => (adjustSpilled curr_mapping u; (TT.enter(aenv, temp, Reg (u, SOME (spillLocOpt frame curr_mapping))), u, us'))
                    | [] =>
                        (* Find a temporary not used in the instruction *)
                        (case spillable aenv al instr
                            of SOME {temp = spillable, ...} =>
                                (case TT.look(aenv, spillable)
                                    of SOME (Reg (r, SOME loc)) => 
                                        let
                                            val aenv' = TT.enter(aenv, spillable, Spilled loc)
                                            val aenv'' = TT.enter(aenv', temp, Reg (r, SOME (spillLocOpt frame curr_mapping)))

                                            (* Spill the unneeded register*)
                                            val _ = emit (A.OPER { assem = "MOV `s0, -" ^ Int.toString (~loc) ^ "(`d0)\n", dst = ["%ebp"], src = [r] })
                                        
                                            (* If needed, read in the previously spilled temp *)
                                            val _ = adjustSpilled curr_mapping r
                                        in
                                        (aenv'', r, [])
                                        end
                                    | _ => ErrorMsg.impossible "register allocation failed - 1")
                            | NONE => ErrorMsg.impossible "register allocation failed - 2"))
        end

    fun getRegInterval aenv us al (i as {temp, ...}) is (instrs as ((_, instr) :: _)) : unit =
        let
            val (aenv', u, us') = getReg aenv us al temp instr
        in
        alloc aenv' us' (insertByFinish i al) is instrs
        end

    (* active is sorted in order of increasing endpoint.
       ins is sorted in order of increasing startpoint *)
    and alloc (aenv : alloced TT.table)
              (useable : F.register list)
              (active : interval list)
              ([] : interval list)
              ([] : (int * Temp.temp A.instr) list) : unit = ()
      | alloc aenv useable active [] ((_, instr) :: instrs) =
                let
                    val (aenv', us') = substReg aenv useable active instr
                in
                alloc aenv' us' active [] instrs
                end
      | alloc aenv us [] ((i as {first, ...}) :: is) (all_assems as ((p, assem) :: assems)) =
        (case p < first
            of true => let val (aenv', us') = substReg aenv us [] assem in alloc aenv' us' [] (i :: is) assems end
             | false => getRegInterval aenv us [] i is all_assems
        )
      | alloc aenv us
                   ((a as {temp = a_temp, first = a_first, finish = a_finish}) :: al)
                   ((i as {temp = i_temp, first = i_first, finish = i_finish}) :: is)
                   (all_assems as ((p, assem) :: assems)) =
            (case p < i_first andalso p <= a_finish
                of true => let val (aenv', us') = substReg aenv us (a :: al) assem in alloc aenv' us' (a :: al) (i :: is) assems end
                 | false =>
                    (case i_first <= a_finish
                        of true => getRegInterval aenv us (a :: al) i is all_assems
                         | false =>
                            case TT.look(aenv, a_temp)
                                of SOME (Reg (r, _)) =>
                                    (* We are finished with the a_temp, so register r can be reused. *)
                                    alloc aenv (r :: us) al (i :: is) all_assems
                                 | SOME (Spilled _) =>  alloc aenv us al (i :: is) all_assems
                                 | NONE => ErrorMsg.impossible "Finished register that was never started"
                    )
            )
    in
    alloc initMap initArgs [] ins
        (ListPair.zip ((List.tabulate (length assem, fn x => x), assem)));
    rev (!ilist)
    end


end