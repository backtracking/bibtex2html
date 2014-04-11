;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  bibtex2html - A BibTeX to HTML translator                             ;
;  Copyright (C) 1997-2014 Jean-Christophe FilliÃ¢tre and Claude MarchÃ©   ;
;                                                                        ;
;  This software is free software; you can redistribute it and/or        ;
;  modify it under the terms of the GNU General Public                   ;
;  License version 2, as published by the Free Software Foundation.      ;
;                                                                        ;
;  This software is distributed in the hope that it will be useful,      ;
;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  ;
;                                                                        ;
;  See the GNU General Public License version 2 for more details         ;
;  (enclosed in the file GPL).                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; bibtex2html.nsi
; Initially created by Aurélien OUDOT
; Maintained by Claude Marché

Name "Bibtex2html"
Icon "icons\install.ico"
OutFile "bibtex2html-${VERSION}-installer.exe"

LicenseText "This application will install Bibtex2html under GNU General Public License"
LicenseData "GPL"
ComponentText "Bibtex2html installation"
DirText "Destination Folder"

InstallDir "$PROGRAMFILES\Bibtex2html"
UninstallText "Do you really want to delete bibtex2html from your system?"
UninstallIcon "icons\uninstall.ico"

; ================================================================

Section "Program"
        SectionIn 1 2
	;messageBox MB_OK "Welcome to the bibtex2html installer!"
	setOutPath $INSTDIR
	File "aux2bib"
	File "bib2bib.exe"
	File "bibtex2html.exe"
SectionEnd

Section -PostInstall
        WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Bibtex2html" "DisplayName" "Bibtex2html (uninstall)"
        WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Bibtex2html" "UninstallString" '"$INSTDIR\bibtex2html-uninstaller.exe"'
        writeUninstaller "bibtex2html-uninstaller.exe"
        WriteRegStr HKLM "Software\Bibtex2html" "Repertoire" '"$INSTDIR"'
SectionEnd

Section "ShortCut"
        SectionIn 2
        setOutPath "$SMPROGRAMS\Bibtex2html"
        CreateShortCut "$SMPROGRAMS\Bibtex2html\bib2bib.lnk" "$INSTDIR\bib2bib.exe"
        CreateShortCut "$SMPROGRAMS\Bibtex2html\bibtex2html.lnk" "$INSTDIR\bibtex2html.exe"
        CreateShortCut "$SMPROGRAMS\Bibtex2html\bibtex2html-uninstaller.lnk" "$INSTDIR\bibtex2html-uninstaller.exe"
	; shortcuts on Desktop -> not needed for bibtex2html
        ;CreateShortCut "$DESKTOP\bib2bib.lnk" "$INSTDIR\bib2bib.exe"
        ;CreateShortCut "$DESKTOP\bibtex2html.lnk" "$INSTDIR\bibtex2html.exe"
SectionEnd

Section "Uninstall"
        DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Bibtex2html"
        DeleteRegKey HKLM "Software\Bibtex2html"
        Delete "$INSTDIR\aux2bib"
        Delete "$INSTDIR\bib2bib.exe"
        Delete "$INSTDIR\bibtex2html.exe"
        Delete "$INSTDIR\bibtex2html-uninstaller.exe"
        RMDir $INSTDIR
        Delete "$SMPROGRAMS\Bibtex2html\*.*"
        RMDir "$SMPROGRAMS\Bibtex2html"
        ;Delete "$DESKTOP\bib2bib.lnk"
        ;Delete "$DESKTOP\bibtex2html.lnk"
SectionEnd


