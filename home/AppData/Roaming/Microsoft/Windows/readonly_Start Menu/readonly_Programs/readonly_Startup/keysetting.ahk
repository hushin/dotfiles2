#Requires AutoHotkey v2.0
#Warn  ;Enable warnings to assist with detecting common errors.
#SingleInstance force
InstallKeybdHook()
#WinActivateForce
#UseHook
DetectHiddenWindows(true)
SetWinDelay(0)
SetTitleMatchMode("RegEx")

;ChangeKeyで左WinキーにF13(0064)を、右WinキーにF14(0065)を割り当てている想定

;Shift  +
;Ctrl   ^
;Alt    !
;Win    #

;特殊なキー http://ahkwiki.net/KeyList

;; LWin を Mac Command キーのように使う
F13 & a::^a
F13 & b::^b
F13 & c::^c
F13 & d::^d
F13 & e::^e
F13 & f::^f
F13 & g::^g
F13 & h:: {
    if GetKeyState("Alt") {
        SendEvent("#{Home}")
        return
    }
    SendEvent("#{Down}")
    return
}
F13 & i::^i
F13 & j::^j
F13 & k::^k
F13 & l::^l
F13 & m:: WinMinimize "A"
F13 & n::^n
F13 & o::^o
F13 & p::^p
F13 & q::!F4
F13 & r::^r
F13 & s::^s
F13 & t::^t
F13 & u::^u
F13 & v::^v
F13 & w::^w
F13 & x::^x
F13 & y::^y
F13 & z::^z

F13 & 1::^1
F13 & 2::^2
F13 & 3::
{
    if GetKeyState("Shift") {
        SendEvent("#{PrintScreen}")
        return
    }
    SendEvent("^3")
    return
}
F13 & 4::
{
    if GetKeyState("Shift") {
        SendEvent("#+s") ;切り取り＆スケッチ
        return
    }
    SendEvent("^4")
    return
}
F13 & 5::^5
F13 & 6::^6
F13 & 7::^7
F13 & 8::^8
F13 & 9::^9
F13 & 0::^0

F13 & Enter::^Enter
F13 & /::^/
F13 & [::
{
    if GetKeyState("Shift") {
        SendEvent("^{PgUp}")
        return
    }
    SendEvent("^[")
    return
}
F13 & ]::
{
    if GetKeyState("Shift") {
        SendEvent("^{PgDn}")
        return
    }
    SendEvent("^]")
    return
}
F13 & +::^= ; 若干自信ない
F13 & -::^-
F13 & ,::^,
F13 & .::^.
; 単語移動
F13 & Right::^Right
F13 & Left::^Left

; 特殊系
F13 & BackSpace:: SendEvent("+{Home}{BackSpace}")

; 仮想デスクトップの移動
^Right::^#Right
^Left::^#Left

;;RWin を 普通Windowsキーのように使う
; スタートメニュー使いたいときは C-Esc を使う
F14 & a::#a
F14 & b::#b
F14 & c::#c
; F14 & d::#d ; デスクトップ表示 誤爆するので無効化
F14 & e::#e
F14 & f::#f
F14 & g::#g
F14 & h::#h
F14 & i::#i
F14 & j::#j
F14 & k::#k
F14 & l::#l
; F14 & m::#m ; すべてのウィンドウの最小化 誤爆するので無効化
F14 & n::#n
F14 & o::#o
F14 & p::#p
F14 & q::#q
F14 & r::#r
F14 & s::#s
F14 & t::#t
F14 & u::#u
F14 & v::#v
F14 & w::#w
F14 & x::#x
F14 & y::#y
F14 & z::#z
F14 & 1::#1
F14 & 2::#2
F14 & 3::#3
F14 & 4::#4
F14 & 5::#5
F14 & 6::#6
F14 & 7::#7
F14 & 8::#8
F14 & 9::#9
F14 & 0::#0
F14 & Left::#Left
F14 & Right::#Right
F14 & Up::#Up
F14 & Down::#Down
F14 & Space::#Space
F14 & .::#.

;; Emacs like
#HotIf !WinActive("ahk_exe (ubuntu2204|bash|emacs|WindowsTerminal).exe")
^p:: SendEvent("{Up}")
^n:: SendEvent("{Down}")
^f:: SendEvent("{Right}")
^b:: SendEvent("{left}")
^a:: SendEvent("{Home}")
^e:: SendEvent("{End}")
^v:: SendEvent("{PgDn}")
!v:: SendEvent("{PgUp}")
!,:: SendEvent("^{Home}")
!.:: SendEvent("^{End}")

+^p:: SendEvent("+{Up}")
+^n:: SendEvent("+{Down}")
+^f:: SendEvent("+{Right}")
+^b:: SendEvent("+{left}")
+^a:: SendEvent("+{Home}")
+^e:: SendEvent("+{End}")
+^v:: SendEvent("+{PgDn}")
+!v:: SendEvent("+{PgUp}")
+!,:: SendEvent("+^{Home}")
+!.:: SendEvent("+^{End}")

+BackSpace:: SendEvent("{Delete}")

^d:: SendEvent("{Delete}")
^h:: SendEvent("{BackSpace}")
^w::^x
^y::^v
^s::^f
^/::^z
^k:: SendEvent("+{End}^x")
^m:: SendEvent("{Enter}")
^j:: SendEvent("{Enter}")
#HotIf

!BackSpace:: SendEvent("^{BackSpace}")

;-----------------------------------------------------------
; IMEの状態をセット
;   SetSts          1:ON / 0:OFF
;   WinTitle="A"    対象Window
;   戻り値          0:成功 / 0以外:失敗
;-----------------------------------------------------------
IME_SET(SetSts, WinTitle := "A") {
    if !(WinActive(WinTitle))
        return -1

    hwnd := WinGetID
    ptrSize := !A_PtrSize ? 4 : A_PtrSize
    cbSize := 4 + 4 + (PtrSize * 6) + 16
    stGTI := Buffer(cbSize, 0)
    NumPut("UInt", cbSize, stGTI, 0)   ;	DWORD   cbSize;
    hwnd := DllCall("GetGUIThreadInfo", "Uint", 0, "Uint", stGTI.Ptr)
        ? NumGet(stGTI, 8 + PtrSize, "UInt") : hwnd

    return DllCall("SendMessage"
        , "UInt", DllCall("imm32\ImmGetDefaultIMEWnd", "Uint", hwnd)
        , "UInt", 0x0283  ;Message : WM_IME_CONTROL
        , "Int", 0x006   ;wParam  : IMC_SETOPENSTATUS
        , "Int", SetSts) ;lParam  : 0 or 1
}

F13:: IME_SET(0)
F14 up:: IME_SET(1)
^[::
{
    SendEvent("{Escape}")
    IME_SET(0)
    return
}

F13 & Tab::AltTab
!Tab:: SendEvent("#{Tab}")

; NOTE: SendEvent で 押しっぱなし対策
; ref. https://did2memo.net/2024/06/11/autohotkey-ctrl-key-is-stuck-sendevent/

; LWin+Click -> Ctrl+Click
F13 & LButton::
{
    MouseGetPos(&x, &y)
    SendEvent("{Ctrl down}{Click %x% %y%}{Ctrl up}")
}

; RWin+Click -> Win+Click
F14 & LButton::
{
    MouseGetPos(&x, &y)
    SendEvent("{LWin Down}{Click %x% %y%}{LWin Up}")
}

; LWin+スクロール -> Ctrl+スクロール
F13 & WheelUp:: SendEvent("{Ctrl down}{WheelUp}{Ctrl up}")
F13 & WheelDown:: SendEvent("{Ctrl down}{WheelDown}{Ctrl up}")

;; Emacs 用の F13 キー設定（Application keyとして送信）
#HotIf WinActive("ahk_exe emacs.exe")
; F13 を AppsKey として送信する
F13 & a:: SendEvent("{AppsKey down}a{AppsKey up}")
F13 & c:: SendEvent("{AppsKey down}c{AppsKey up}")
F13 & v:: SendEvent("{AppsKey down}v{AppsKey up}")
F13 & x:: SendEvent("{AppsKey down}x{AppsKey up}")
F13 & z:: SendEvent("{AppsKey down}z{AppsKey up}")
F13 & s:: SendEvent("{AppsKey down}s{AppsKey up}")
F13 & o:: SendEvent("{AppsKey down}o{AppsKey up}")
F13 & n:: SendEvent("{AppsKey down}n{AppsKey up}")
F13 & w:: SendEvent("{AppsKey down}w{AppsKey up}")
F13 & f:: SendEvent("{AppsKey down}f{AppsKey up}")
F13 & q:: SendEvent("{AppsKey down}q{AppsKey up}")
#HotIf

; sound volume
RShift & F12::
{
    SoundSetVolume("+5")
    SoundPlay("*64")
    return
}

RShift & F11::
{
    SoundSetVolume(-5)
    SoundPlay("*64")
    return
}

RShift & F10::
{
    SendEvent("{Volume_Mute}")
    SoundPlay("*64")
    return
}

; WindowsTerminal 切り替え
F13 & `;::
{
    terminal := WinExist("ahk_class CASCADIA_HOSTING_WINDOW_CLASS")
    ; terminal := WinExist("ahk_exe WindowsTerminal.exe")
    if (terminal) {
        active := WinActive("ahk_id " terminal)
        if (active)
            WinMinimize("ahk_id " active)
        else
            WinActivate("ahk_id " terminal)
    }
    else
        Run("wt.exe")
    return
}

#HotIf WinActive("ahk_exe (Code.exe|Code - Insiders.exe)")
; Ctrl + ` でターミナルを開くように設定しておく
; AHK で ctrl + ` を ctrl + F22 に変換
; 変換 = vkF3、無変換 = vkF4 に該当
; ref https://ahkwiki.net/KeyList
^vkF3:: SendEvent("^{F22}")
^vkF4:: SendEvent("^{F22}")
#HotIf

#HotIf WinActive("ahk_exe WindowsTerminal.exe")
F13 & n:: SendEvent("^+n")
F13 & t:: SendEvent("^+t")
F13 & w:: SendEvent("^+w")
F13 & f:: SendEvent("^+f")
F13 & d:: SendEvent("!+d")
#HotIf

#HotIf WinActive("ahk_exe chrome.exe")
F13 & y:: SendEvent("^h") ; 履歴
F13 & i:: {
    if GetKeyState("Alt") {
        ; DevTools を開く
        SendEvent("^+i")
        return
    }
    SendEvent("^i")
    return
}
#HotIf

; Kindle ページめくり
!]::
{
    hWnd := WinExist(".*Kindle for PC.*")
    if (hWnd) {
        ControlSend("{Right}", hWnd)
    }
}
![::
{
    hWnd := WinExist(".*Kindle for PC.*")
    if (hWnd) {
        ControlSend("{Left}", hWnd)
    }
}

#HotIf WinActive("ahk_exe Kindle.exe")
[:: SendEvent("{Left}")
]:: SendEvent("{Right}")
#HotIf

;; 定型文

; 高速に文字列を挿入する
; ref: https://did2memo.net/2014/03/19/autohotkey-insert-text/
InsertText(Content) {
    cb_bk := ClipboardAll()
    A_Clipboard := Content
    SendEvent("^v")
    Sleep(100)
    A_Clipboard := cb_bk
}

; 終了文字なしで発動
#HotString *

::`;dd::
{
    TimeString := FormatTime(, "yyyyMMdd")
    InsertText(TimeString)
    return
}

::`;yoro::
{
    InsertText("よろしくお願いいたします。")
    return
}

::`;cb::
{
    InsertText("```````n" A_Clipboard "`n```````n")
    return
}

::`;ahk::
{
    InsertText("AutoHotkey")
    return
}

::`;x::
{
    InsertText("𝕏")
    return
}

::`;daily::
{
    prompt := "
(
以下は私が日記を文字起こししたものです。日記形式に修正してください。

- 要約したタイトルをつける
- 句読点や誤字があるので、文脈を推測して修正する
- 「えっと」「なんか」など 意味のない用語は無視
- 話題ごとに改行を挟む
- 重複した内容はまとめる

---

)"
    InsertText(prompt)
    return
}
