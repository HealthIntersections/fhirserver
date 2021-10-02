## run this from any directory, but it's tested in ~/work

git clone https://github.com/dezlov/PascalTZ
git clone https://github.com/grahamegrieve/delphi-markdown
git clone https://github.com/mriscoc/extrasyn
git clone https://github.com/grahamegrieve/HtmlViewer
git clone https://github.com/grahamegrieve/lazarus-ide-tester
git clone https://github.com/Xor-el/QRCodeGenLib4Pascal
git clone https://github.com/grahamegrieve/fhirserver

## todo: how to run lazbuild? (path issue)

lazbuild --add-package PascalTZ/package/pascaltz.lpk
lazbuild --add-package extrasyn/extrahighlighters.lpk
lazbuild --add-package extrasyn/extrahighlighters_dsgn.lpk
lazbuild --add-package lazarus-ide-tester/package/idetester.dpk
lazbuild --add-package lazarus-ide-tester/ide/idetester_dsgn.lpk
lazbuild --add-package HtmlViwer/package/FrameViewer09.lpk
lazbuild --add-package delphi-markdown/packages/markdownengine.lpk
lazbuild --add-package QRCodeGenLib4Pascal/QRCodeGenLib/src/Packages/FPC/QRCodeGenLib4PascalPackage.lpk
