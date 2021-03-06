; iNES header

.segment "HEADER"

INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; CHR ROM

.segment "CHARS"
.incbin "yokan.chr"

; vectors placed at top 6 bytes of memory area

.segment "VECTORS"
.word nmi
.word reset
.word irq

; reset routine

.segment "STARTUP"
reset:
	sei       ; mask interrupts
	lda #0
	sta $2000 ; disable NMI
	sta $2001 ; disable rendering
	sta $4015 ; disable APU sound
	sta $4010 ; disable DMC IRQ
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; initialize stack
	; wait for first vblank
	bit $2002
	:
		bit $2002
		bpl :-
	; clear all RAM to 0
	lda #0
	ldx #0
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; place all sprites offscreen at Y=255
	lda #255
	ldx #0
	:
		sta oam, X
		inx
		inx
		inx
		inx
		bne :-
	; wait for second vblank
	:
		bit $2002
		bpl :-
	; NES is initialized, ready to begin!
	; enable the NMI for graphical updates, and jump to our main program
	lda #%10010000
	sta $2000
	lda #%00011110
	sta $2001
	jmp main

;
; nmi routine
;

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_count:      .res 1 ; is incremented every NMI
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
jicho_x:        .res 1 ; 次長X座標
walk_count:     .res 1 ; アニメーション用歩行カウンタ
jicho_y:        .res 1 ; 次長Y座標
jicho_type:     .res 1 ; 表示中キャラクタパターン
pad:            .res 2 ; コントローラ入力値
work1:          .res 1

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "CODE"
nmi:
	; save registers
	pha
	txa
	pha
	tya
	pha
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp @nmi_end
	:
	lda #1
	sta nmi_lock
	; increment frame counter
	inc nmi_count
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp @ppu_update_end
	:
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta $2001
		ldx #0
		stx nmi_ready
		jmp @ppu_update_end
	:
	; sprite OAM DMA
	ldx #0
	stx $2003
	lda #2
	sta $4014

@ppu_update_end:
	; if this engine had music/sound, this would be a good place to play it
	; unlock re-entry flag
	lda #0
	sta nmi_lock


	; コントローラ読み込み
	lda #1
	sta $4016
	lsr ; A ← 0
	tax ; X ← 0
	sta $4016
	ldy #8
	:
		pha
		lda $4016, X
		and #%00000011 ; Pad3(or4) & Pad1(or2)
		cmp #%00000001
		pla
		rol
		dey
		bne :-
	sta pad, X

	; 右移動
	lda pad
	and #%00000001
	beq @exit_move_r
	inc jicho_x
	lda jicho_type
	cmp #0
	beq :+
	cmp #2
	beq :+
	lda #0
	sta jicho_type
	lda #0
	sta walk_count
	jmp @exit_move_r
	:
	; パターン切り替え判定
	inc walk_count
	lda walk_count
	cmp #8
	bne @exit_move_r
	; パターン切り替え
	lda #0
	sta walk_count
	lda jicho_type
	cmp #0
	beq :+
	lda #0
	sta jicho_type
	jmp @exit_move_r
	:
	lda #2
	sta jicho_type
@exit_move_r:

	; 左移動
	lda pad
	and #%00000010
	beq @exit_move_l
	dec jicho_x
	lda jicho_type
	cmp #1
	beq :+
	cmp #3
	beq :+
	lda #1
	sta jicho_type
	lda #0
	sta walk_count
	jmp @exit_move_l
	:
	; パターン切り替え判定
	inc walk_count
	lda walk_count
	cmp #8
	bne @exit_move_l
	; パターン切り替え
	lda #0
	sta walk_count
	lda jicho_type
	cmp #1
	beq :+
	lda #1
	sta jicho_type
	jmp @exit_move_l
	:
	lda #3
	sta jicho_type
@exit_move_l:

	; ; OAMに設定
	; 次長
	lda jicho_type
	asl
	asl
	clc
	adc jicho_type
	tay ; Y ← jicho_type x 5
	; 反転設定
	lda jicho_pattern, Y
	iny
	sta work1
	; 左上
	lda jicho_y
	ldx #0
	sta oam, X ; Y座標
	inx
	lda jicho_pattern, Y
	iny
	sta oam, X ; パターン
	inx
	lda work1
	and #%11000000
	sta oam, X ; 属性
	inx
	lda jicho_x
	sta oam, X ; X座標
	inx
	; 右上
	lda jicho_y
	sta oam, X
	inx
	lda jicho_pattern, Y
	iny
	sta oam, X
	inx
	lda work1
	asl
	asl
	and #%11000000
	sta oam, X
	inx
	lda jicho_x
	clc
	adc #8
	sta oam, X
	inx
	; 左下
	lda jicho_y
	clc
	adc #8
	sta oam, X
	inx
	lda jicho_pattern, Y
	iny
	sta oam, X
	inx
	lda work1
	asl
	asl
	and #%11000000
	sta oam, X
	inx
	lda jicho_x
	sta oam, X
	inx
	; 右下
	lda jicho_y
	clc
	adc #8
	sta oam, X
	inx
	lda jicho_pattern, Y
	iny
	sta oam, X
	inx
	lda work1
	asl
	asl
	and #%11000000
	sta oam, X
	inx
	lda jicho_x
	clc
	adc #8
	sta oam, X
	inx


@nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti

; irq

.segment "CODE"
irq:
	rti

ppu_update:
	lda #1
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; main

.segment "CODE"
main:
	; setup 

	;; パレットテーブル設定
	; BG
	lda	#$3f
	sta	$2006
	lda	#$00
	sta	$2006
	ldy	#$10
	:
		lda	#$0C
		sta	$2007
		dey
		bne	:-
	; OBJ
	lda	#$3f
	sta	$2006
	lda	#$10
	sta	$2006
	lda	#$00
	sta	$2007
	lda	#$14
	sta	$2007
	lda	#$20
	sta	$2007
	lda	#$01
	sta	$2007

	;;
	lda #120
	sta jicho_x
	lda #160
	sta jicho_y
	lda #0
	sta jicho_type

	lda #1
	sta nmi_ready

	; main loop
@loop:
	jmp @loop

; 次長パターン
jicho_pattern:
	; 反転指定, パターン1, パターン2, パターン3, パターン4
	.byte	$00, $00, $01, $10, $11 ; つまようじ装備中移動(1) 右向き
	.byte	$55, $01, $00, $11, $10 ; つまようじ装備中移動(1) 左向き
	.byte	$00, $02, $03, $12, $13 ; つまようじ装備中移動(2) 右向き
	.byte	$55, $03, $02, $13, $12 ; つまようじ装備中移動(2) 左向き
