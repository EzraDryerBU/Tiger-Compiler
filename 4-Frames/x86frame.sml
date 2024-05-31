structure X86Frame : FRAME =
struct
    datatype access = InFrame of int | InReg of Temp.temp
    type frame = {name : Temp.label, formals : access list, frameOff : int ref}

    val wordSize = 4
    fun incNextSpot ns = ns := !ns + wordSize
    fun decFrameOff ns = ns := !ns - wordSize
    fun name {name, formals, frameOff} =  name
    fun formals {name, formals, frameOff} = formals
    (*Locals move down, change frameOff*)
    fun allocLocal {name, formals, frameOff} escape = (
      case escape of 
        true => (
          let
            val temp = !frameOff
            val _ = decFrameOff frameOff
            val newAccess = InFrame(temp)

          in
            newAccess
          end
        )
        | false => (InReg(Temp.newtemp()))
    )

    fun newFrame {name, formals} = (
      let
        val nextSpot = ref 0
        fun determineFormals f = (
          case f of
            true => (
              let
                val tempHolder = !nextSpot 
              in 
                incNextSpot nextSpot;
                InFrame(tempHolder)
              end
              )
            | false => (InReg(Temp.newtemp()))   
        )        
        val newFormals = map determineFormals formals
      in
        {name=name, formals=newFormals, frameOff=ref (~wordSize)}
      end
      )
    fun printAccess (InFrame w) = ("(InFrame " ^ Int.toString w ^ ")")
      | printAccess (InReg _) = "InReg"
end