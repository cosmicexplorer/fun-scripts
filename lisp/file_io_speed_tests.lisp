(defvar test-in)
(defvar test-out)
(defvar bool)
(setf test-in (make-sequence 'string (* (expt 10 6) 10) :initial-element #\A))
(setf test-out (make-sequence 'string (* (expt 10 6) 10) :initial-element #\Nul))
(setf bool nil)
(defun test-loop ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (string test-in test-out) (boolean bool))
  (loop for index fixnum from 0 to (- (length test-in) 1)
     do (progn
          (if bool
              (setf (aref test-out index) (aref test-in index)))
          (setf bool (not bool)))))

(defvar cur-count)
(setf cur-count 0)
(declaim (inline map-fun))
(defun map-fun (char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (character char) (fixnum cur-count) (string test-out))
  (if bool
      (setf (aref test-out cur-count) char))
  (setf bool (not bool))
  (the fixnum (incf cur-count)))
(defun test-map ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (string test-in) (fixnum cur-count))
  (declare (inline map-fun))
  (map nil #'map-fun test-in)
  (setf cur-count 0))




(declaim (inline find-prime-factor))
(defun find-prime-factor (num)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (fixnum num))
  (let ((is-factor nil))
    (declare (boolean is-factor))
    (loop for i fixnum from 2 to num
       until is-factor
       do (if (= (nth-value 1 (floor num i)) 0)
              (setf is-factor t))
       finally (return (- i 1)))))

;;; 15485863
(time (find-prime-factor 15485863))     ; ~.184 seconds

;;;;; RESULTS
(print "CL-USER> (progn
           (time (test-loop))
           (time (test-map)))
Evaluation took:
  391.740 seconds of real time
  392.086627 seconds of total run time (392.073294 user, 0.013333 system)
  100.09% CPU
  625,215,073,400 processor cycles
  2,523,088 bytes consed
  
Evaluation took:
  393.350 seconds of real time
  393.696627 seconds of total run time (393.679960 user, 0.016667 system)
  [ Run times consist of 0.030 seconds GC time, and 393.667 seconds non-GC time. ]
  100.09% CPU
  627,786,205,904 processor cycles
  2,496,416 bytes consed
  
NIL")


;;; DISASSEMBLE

;; disassembly for TEST-LOOP
;; Size: 323 bytes. Origin: #x1002E60FFF
;; 0FFF:       488B1592FFFFFF   MOV RDX, [RIP-110]             ; 'TEST-IN
;; no-arg-parsing entry point
;; 1006:       8B42F5           MOV EAX, [RDX-11]
;; 1009:       498B0404         MOV RAX, [R12+RAX]
;; 100D:       83F861           CMP EAX, 97
;; 1010:       480F4442F9       CMOVEQ RAX, [RDX-7]
;; 1015:       488B40F9         MOV RAX, [RAX-7]
;; 1019:       4883E802         SUB RAX, 2
;; 101D:       488945F8         MOV [RBP-8], RAX
;; 1021:       4D31C0           XOR R8, R8
;; 1024:       EB72             JMP L2
;; 1026:       660F1F840000000000 NOP
;; 102F:       90               NOP
;; 1030: L0:   488B1569FFFFFF   MOV RDX, [RIP-151]             ; 'BOOL
;; 1037:       8B42F5           MOV EAX, [RDX-11]
;; 103A:       498B0404         MOV RAX, [R12+RAX]
;; 103E:       83F861           CMP EAX, 97
;; 1041:       480F4442F9       CMOVEQ RAX, [RDX-7]
;; 1046:       483D17001020     CMP RAX, 537919511
;; 104C:       755B             JNE L3
;; 104E: L1:   488B154BFFFFFF   MOV RDX, [RIP-181]             ; 'BOOL
;; 1055:       8B42F5           MOV EAX, [RDX-11]
;; 1058:       498B0404         MOV RAX, [R12+RAX]
;; 105C:       83F861           CMP EAX, 97
;; 105F:       480F4442F9       CMOVEQ RAX, [RDX-7]
;; 1064:       483D17001020     CMP RAX, 537919511
;; 106A:       B817001020       MOV EAX, 537919511
;; 106F:       41BB4F001020     MOV R11D, 537919567            ; T
;; 1075:       490F44C3         CMOVEQ RAX, R11
;; 1079:       488B0D20FFFFFF   MOV RCX, [RIP-224]             ; 'BOOL
;; 1080:       8B51F5           MOV EDX, [RCX-11]
;; 1083:       498D541407       LEA RDX, [R12+RDX+7]
;; 1088:       837AF961         CMP DWORD PTR [RDX-7], 97
;; 108C:       480F44D1         CMOVEQ RDX, RCX
;; 1090:       488942F9         MOV [RDX-7], RAX
;; 1094:       4983C002         ADD R8, 2
;; 1098: L2:   4C3B45F8         CMP R8, [RBP-8]
;; 109C:       7E92             JLE L0
;; 109E:       BA17001020       MOV EDX, 537919511
;; 10A3:       488BE5           MOV RSP, RBP
;; 10A6:       F8               CLC
;; 10A7:       5D               POP RBP
;; 10A8:       C3               RET
;; 10A9: L3:   4C8945F0         MOV [RBP-16], R8
;; 10AD:       488B15F4FEFFFF   MOV RDX, [RIP-268]             ; 'TEST-OUT
;; 10B4:       448B4AF5         MOV R9D, [RDX-11]
;; 10B8:       4F8B0C0C         MOV R9, [R12+R9]
;; 10BC:       4183F961         CMP R9D, 97
;; 10C0:       4C0F444AF9       CMOVEQ R9, [RDX-7]
;; 10C5:       4C894DE8         MOV [RBP-24], R9
;; 10C9:       4D8BD0           MOV R10, R8
;; 10CC:       4C8955E0         MOV [RBP-32], R10
;; 10D0:       488B15C1FEFFFF   MOV RDX, [RIP-319]             ; 'TEST-IN
;; 10D7:       8B42F5           MOV EAX, [RDX-11]
;; 10DA:       498B0404         MOV RAX, [R12+RAX]
;; 10DE:       83F861           CMP EAX, 97
;; 10E1:       480F4442F9       CMOVEQ RAX, [RDX-7]
;; 10E6:       498BF8           MOV RDI, R8
;; 10E9:       488D5C24F0       LEA RBX, [RSP-16]
;; 10EE:       4883EC18         SUB RSP, 24
;; 10F2:       488BD0           MOV RDX, RAX
;; 10F5:       488B05B4FEFFFF   MOV RAX, [RIP-332]             ; #<FDEFINITION object for SB-KERNEL:HAIRY-DATA-VECTOR-REF>
;; 10FC:       B904000000       MOV ECX, 4
;; 1101:       48892B           MOV [RBX], RBP
;; 1104:       488BEB           MOV RBP, RBX
;; 1107:       FF5009           CALL QWORD PTR [RAX+9]
;; 110A:       4C8B55E0         MOV R10, [RBP-32]
;; 110E:       4C8B4DE8         MOV R9, [RBP-24]
;; 1112:       488BF2           MOV RSI, RDX
;; 1115:       488D5C24F0       LEA RBX, [RSP-16]
;; 111A:       4883EC18         SUB RSP, 24
;; 111E:       498BD1           MOV RDX, R9
;; 1121:       498BFA           MOV RDI, R10
;; 1124:       488B058DFEFFFF   MOV RAX, [RIP-371]             ; #<FDEFINITION object for SB-KERNEL:HAIRY-DATA-VECTOR-SET>
;; 112B:       B906000000       MOV ECX, 6
;; 1130:       48892B           MOV [RBX], RBP
;; 1133:       488BEB           MOV RBP, RBX
;; 1136:       FF5009           CALL QWORD PTR [RAX+9]
;; 1139:       4C8B45F0         MOV R8, [RBP-16]
;; 113D:       E90CFFFFFF       JMP L1
NIL
;;; 81 LINES

;;; ----------------------

;; disassembly for TEST-MAP
;; Size: 246 bytes. Origin: #x100300715F
;; 15F:       488B0592FFFFFF   MOV RAX, [RIP-110]              ; 'TEST-IN
 ;                                                             ; no-arg-parsing entry point
;; 166:       8B48F5           MOV ECX, [RAX-11]
;; 169:       498B0C0C         MOV RCX, [R12+RCX]
;; 16D:       83F961           CMP ECX, 97
;; 170:       480F4448F9       CMOVEQ RCX, [RAX-7]
;; 175:       488BD1           MOV RDX, RCX
;; 178:       488B59F9         MOV RBX, [RCX-7]
;; 17C:       488BF3           MOV RSI, RBX
;; 17F:       8D41F1           LEA EAX, [RCX-15]
;; 182:       A80F             TEST AL, 15
;; 184:       0F85C1000000     JNE L4
;; 18A:       8B41F1           MOV EAX, [RCX-15]
;; 18D:       3C81             CMP AL, -127
;; 18F:       7408             JEQ L0
;; 191:       3CE9             CMP AL, -23
;; 193:       0F82B2000000     JB L4
;; 199: L0:   488D5C24F0       LEA RBX, [RSP-16]
;; 19E:       4883EC18         SUB RSP, 24
;; 1A2:       31FF             XOR EDI, EDI
;; 1A4:       488B0555FFFFFF   MOV RAX, [RIP-171]              ; #<FDEFINITION object for SB-KERNEL:%WITH-ARRAY-DATA>
;; 1AB:       B906000000       MOV ECX, 6
;; 1B0:       48892B           MOV [RBX], RBP
;; 1B3:       488BEB           MOV RBP, RBX
;; 1B6:       FF5009           CALL QWORD PTR [RAX+9]
;; 1B9:       488BE3           MOV RSP, RBX
;; 1BC:       488BCF           MOV RCX, RDI
;; 1BF:       488BDE           MOV RBX, RSI
;; 1C2: L1:   488955F8         MOV [RBP-8], RDX
;; 1C6:       48894DE8         MOV [RBP-24], RCX
;; 1CA:       4829CB           SUB RBX, RCX
;; 1CD:       48895DF0         MOV [RBP-16], RBX
;; 1D1:       31F6             XOR ESI, ESI
;; 1D3:       EB65             JMP L3
;; 1D5:       660F1F840000000000 NOP
;; 1DE:       6690             NOP
;; 1E0: L2:   488975E0         MOV [RBP-32], RSI
;; 1E4:       488BFE           MOV RDI, RSI
;; 1E7:       48037DE8         ADD RDI, [RBP-24]
;; 1EB:       488D5C24F0       LEA RBX, [RSP-16]
;; 1F0:       4883EC18         SUB RSP, 24
;; 1F4:       488B55F8         MOV RDX, [RBP-8]
;; 1F8:       488B0509FFFFFF   MOV RAX, [RIP-247]              ; #<FDEFINITION object for SB-KERNEL:HAIRY-DATA-VECTOR-REF>
;; 1FF:       B904000000       MOV ECX, 4
;; 204:       48892B           MOV [RBX], RBP
;; 207:       488BEB           MOV RBP, RBX
;; 20A:       FF5009           CALL QWORD PTR [RAX+9]
;; 20D:       488D5C24F0       LEA RBX, [RSP-16]
;; 212:       4883EC18         SUB RSP, 24
;; 216:       488B05F3FEFFFF   MOV RAX, [RIP-269]              ; #<FDEFINITION object for MAP-FUN>
;; 21D:       B902000000       MOV ECX, 2
;; 222:       48892B           MOV [RBX], RBP
;; 225:       488BEB           MOV RBP, RBX
;; 228:       FF5009           CALL QWORD PTR [RAX+9]
;; 22B:       480F42E3         CMOVB RSP, RBX
;; 22F:       488B75E0         MOV RSI, [RBP-32]
;; 233:       488D4E02         LEA RCX, [RSI+2]
;; 237:       488BF1           MOV RSI, RCX
;; 23A: L3:   483B75F0         CMP RSI, [RBP-16]
;; 23E:       7CA0             JL L2
;; 240:       BA17001020       MOV EDX, 537919511
;; 245:       488BE5           MOV RSP, RBP
;; 248:       F8               CLC
;; 249:       5D               POP RBP
;; 24A:       C3               RET
;; 24B: L4:   488BD1           MOV RDX, RCX
;; 24E:       31C9             XOR ECX, ECX
;; 250:       E96DFFFFFF       JMP L1
NIL
;;; 69 LINES
