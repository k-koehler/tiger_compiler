structure RiscVGen : CODEGEN =
struct
  structure Frame = RiscVFrame

  structure A = Assem
  structure E = ErrorMsg
  structure F = RiscVFrame
  structure M = Temp
  structure R = Tree
  structure S = Symbol
  structure T = Temp
        
  fun codegen frame stm =
    let 
      val ilist = ref []

      fun int i = if i < 0 then "-" ^ Int.toString(~i) else Int.toString(i)

      val calldefs = map #2 F.callersaves

      fun emit x = ilist := x :: !ilist

      fun result gen = 
        let 
          val t = T.newtemp()
        in ( gen t; t )
        end

      fun munchRelOp R.EQ  = "beq"
        | munchRelOp R.NE  = "bne"
        | munchRelOp R.LT  = "blt"
        | munchRelOp R.GT  = "bgt"
        | munchRelOp R.LE  = "ble"
        | munchRelOp R.GE  = "bge"
        | munchRelOp R.ULT = "bltu"
        | munchRelOp R.ULE = "bleu"
        | munchRelOp R.UGT = "bgtu"
        | munchRelOp R.UGE = "bgeu"

      and munchBinOp R.PLUS    = "add"
        | munchBinOp R.MINUS   = "sub"
        | munchBinOp R.MUL     = "mul"
        | munchBinOp R.DIV     = "div"
        | munchBinOp R.AND     = "and"
        | munchBinOp R.OR      = "or"
        | munchBinOp R.LSHIFT  = "sll"
        | munchBinOp R.RSHIFT  = "srl"
        | munchBinOp R.ARSHIFT = "sra"
        | munchBinOp R.XOR     = "xor"

      and munchStm (R.SEQ (s1, s2)) = (munchStm s1; munchStm s2)
        | munchStm (R.MOVE (R.MEM (R.BINOP(R.PLUS, R.CONST leftOperand, rightOperand)), exp)) = emit (A.OPER { assem = "sw `s1, " ^ int(leftOperand) ^ "(`s0)\n", src = [munchExp rightOperand, munchExp exp], dst = [], jump = NONE })
        | munchStm (R.MOVE (R.MEM (R.BINOP(R.PLUS, leftOperand, R.CONST rightOperand)), exp)) = emit (A.OPER { assem = "sw `s1, " ^ int(rightOperand) ^ "(`s0)\n", src = [munchExp leftOperand, munchExp exp], dst = [], jump = NONE })
        | munchStm (R.MOVE (R.MEM (R.BINOP(R.MINUS, R.CONST leftOperand, rightOperand)), exp)) = emit (A.OPER { assem = "sw `s1, " ^ int(leftOperand) ^ "(`s0)\n", src = [munchExp rightOperand, munchExp exp], dst = [], jump = NONE })
        | munchStm (R.MOVE (R.MEM (R.BINOP(R.MINUS, leftOperand, R.CONST rightOperand)), exp)) = emit (A.OPER { assem = "sw `s1, " ^ int(rightOperand) ^ "(`s0)\n", src = [munchExp leftOperand, munchExp exp], dst = [], jump = NONE })
        | munchStm (R.MOVE (R.MEM exp1, exp2)) = emit (A.OPER { assem = "sw `s1, 0(`s0)\n", src = [munchExp exp1, munchExp exp2], dst = [], jump = NONE })
        | munchStm (R.MOVE (exp, R.MEM (R.BINOP(R.PLUS, R.CONST leftOperand, rightOperand)))) = emit (A.OPER { assem = "lw `d0, " ^ int(leftOperand) ^ "(`s0)\n", src = [munchExp rightOperand], dst = [munchExp exp], jump = NONE })
        | munchStm (R.MOVE (exp, R.MEM (R.BINOP(R.PLUS, leftOperand, R.CONST rightOperand)))) = emit (A.OPER { assem = "lw `d0, " ^ int(rightOperand) ^ "(`s0)\n", src = [munchExp leftOperand], dst = [munchExp exp], jump = NONE })
        | munchStm (R.MOVE (exp, R.MEM (R.BINOP(R.MINUS, R.CONST leftOperand, rightOperand)))) = emit (A.OPER { assem = "lw `d0, " ^ int(leftOperand) ^ "(`s0)\n", src = [munchExp rightOperand], dst = [munchExp exp], jump = NONE })
        | munchStm (R.MOVE (exp, R.MEM (R.BINOP(R.MINUS, leftOperand, R.CONST rightOperand)))) = emit (A.OPER { assem = "lw `d0, " ^ int(rightOperand) ^ "(`s0)\n", src = [munchExp leftOperand], dst = [munchExp exp], jump = NONE })
        | munchStm (R.MOVE (exp1, R.MEM exp2)) = emit (A.OPER { assem = "lw `d0, 0(`s0)\n", src = [munchExp exp2], dst = [munchExp exp1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.PLUS, R.CONST leftOperand, rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.PLUS, leftOperand, R.CONST rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.AND, R.CONST leftOperand, rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.AND, leftOperand, R.CONST rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.OR, R.CONST leftOperand, rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.OR, leftOperand, R.CONST rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.LSHIFT, R.CONST leftOperand, rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.LSHIFT, leftOperand, R.CONST rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.RSHIFT, R.CONST leftOperand, rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.RSHIFT, leftOperand, R.CONST rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.ARSHIFT, R.CONST leftOperand, rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.ARSHIFT, leftOperand, R.CONST rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.XOR, R.CONST leftOperand, rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper as R.XOR, leftOperand, R.CONST rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (e1, R.BINOP (oper, leftOperand, rightOperand))) = emit (A.OPER { assem = (munchBinOp oper) ^ " `d0, `s0, `s1\n", src = [munchExp leftOperand, munchExp rightOperand], dst = [munchExp e1], jump = NONE })
        | munchStm (R.MOVE (R.TEMP e1, R.TEMP e2)) = if (e1 = e2) then () else emit (A.MOVE { assem = "mv `d0, `s0\n", src = e2, dst = e1})
        | munchStm (R.MOVE (R.TEMP label, R.CONST e1)) = emit (A.OPER { assem = "li `d0, " ^ int(e1) ^ "\n", src = [], dst = [label], jump=NONE})
        | munchStm (R.MOVE (R.TEMP label, e1)) = emit (A.MOVE { assem = "mv `d0, `s0\n", src = munchExp e1, dst = label})
        | munchStm (R.MOVE (e1, e2)) = emit (A.MOVE { assem = "mv `d0, `s0\n", src = munchExp e2, dst = munchExp e1})
        | munchStm (R.JUMP (R.NAME(name), labelList)) = emit (A.OPER{assem="j " ^ S.name name ^ "\n", src=[], dst=[], jump=SOME(labelList)})
        | munchStm (R.JUMP (e, labelList)) = emit (A.OPER{assem="jr `j0, \n", src=[munchExp e], dst=[], jump=SOME(labelList)})
        | munchStm (R.CJUMP (relop, leftExp, rightExp, label1, label2)) = emit (A.OPER{assem= munchRelOp relop ^ " `s0, `s1, " ^ S.name label1 ^ "\n", src=[munchExp leftExp, munchExp rightExp], dst=[], jump=SOME([label1, label2])})
        | munchStm (R.EXP e) = (munchExp e; ())
        | munchStm (R.LABEL label) = emit (A.LABEL { assem = (S.name label) ^ ": \n", lab= label })

      and munchExp (R.CONST 0) = F.ZERO
        | munchExp (R.CONST i) = result (fn r => emit (A.OPER { assem = "li `d0, " ^ int(i) ^ "\n", src = [], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.PLUS, R.CONST leftOperand, rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.PLUS, leftOperand, R.CONST rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.AND, R.CONST leftOperand, rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.AND, leftOperand, R.CONST rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.OR, R.CONST leftOperand, rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.OR, leftOperand, R.CONST rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.LSHIFT, R.CONST leftOperand, rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.LSHIFT, leftOperand, R.CONST rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.RSHIFT, R.CONST leftOperand, rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.RSHIFT, leftOperand, R.CONST rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.ARSHIFT, R.CONST leftOperand, rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.ARSHIFT, leftOperand, R.CONST rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.XOR, R.CONST leftOperand, rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(leftOperand) ^ "\n", src = [munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper as R.XOR, leftOperand, R.CONST rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ "i `d0, `s0, " ^ int(rightOperand) ^ "\n", src = [munchExp leftOperand], dst = [r], jump = NONE }))
        | munchExp (R.BINOP(oper, leftOperand, rightOperand)) = result (fn r => emit (A.OPER { assem = (munchBinOp oper) ^ " `d0, `s0, `s1\n", src = [munchExp leftOperand, munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.MEM (R.BINOP(R.PLUS, R.CONST leftOperand, rightOperand))) = result (fn r => emit (A.OPER { assem = "lw `d0, " ^ int(leftOperand) ^ "(`s0)\n", src = [munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.MEM (R.BINOP(R.PLUS, leftOperand, R.CONST rightOperand))) = result (fn r => emit (A.OPER { assem = "lw `d0, " ^ int(rightOperand) ^ "(`s0)\n", src = [munchExp leftOperand], dst = [r], jump = NONE }))
        | munchExp (R.MEM (R.BINOP(R.MINUS, R.CONST leftOperand, rightOperand))) = result (fn r => emit (A.OPER { assem = "lw `d0, " ^ int(leftOperand) ^ "(`s0)\n", src = [munchExp rightOperand], dst = [r], jump = NONE }))
        | munchExp (R.MEM (R.BINOP(R.MINUS, leftOperand, R.CONST rightOperand))) = result (fn r => emit (A.OPER { assem = "lw `d0, " ^ int(rightOperand) ^ "(`s0)\n", src = [munchExp leftOperand], dst = [r], jump = NONE }))
        | munchExp (R.MEM (expression)) = result (fn r => emit (A.OPER { assem = "lw `d0, 0(`s0)\n", src = [munchExp expression], dst = [r], jump = NONE }))
        | munchExp (R.CALL (R.NAME funcName, args)) = result (fn r => emit (A.OPER { assem = "jal " ^ S.name funcName ^ "\n", src = munchArgs(0, args), dst = r::calldefs, jump=NONE}))
        | munchExp (R.CALL (funcNameExp, args)) = result (fn r => emit (A.OPER { assem = "jal `s0 \n", src = (munchExp funcNameExp)::munchArgs(0, args), dst = r::calldefs, jump=NONE}))
        | munchExp (R.ESEQ(statment, expression)) = (munchStm statment; munchExp expression)
        | munchExp (R.NAME l) = result (fn r => emit (A.OPER { assem = "la `d0 " ^ (S.name l) ^ "\n", src = [], dst = [r], jump = NONE }))
        | munchExp (R.TEMP t) = t

      and munchArgs (i, es) =
        let 
          val {name,formals,sp} = frame
          fun munchArg (e, F.InFrame n, (i, args)) = 
              let
                val _ = print("I came here 119\n")
                val src = munchExp e 
              in ( emit (A.OPER { assem = "I came here\nsw `s1, " ^ int(n * F.wordSize) ^ "(`s0`)\n", src = [F.FP, src], dst = [], jump  = NONE }); (i+1, src::args))
              end
            | munchArg (e, F.InReg t, (i, args)) = 
              let 
                val _ = print("I came here 125\n")
                val src = munchExp e
              in ( emit (A.MOVE { assem="I came here\nmv `d0, `s0\n", src=src, dst=t }); (i+1, src::args))
              end
        in 
          let 
            val (_, args) = ListPair.foldl munchArg (i, []) (es, formals)
          in args
          end
        end
  in ( munchStm stm; rev (!ilist))
  end
end
