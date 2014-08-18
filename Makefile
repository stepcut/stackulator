view.js: Stackulator.idr Parser.idr Model.idr HtmlView.idr
	idris -p effects -p lightyear -p iquery --codegen javascript HtmlView.idr -o view.js +RTS -M4G

serve: view.js
	@echo "listening."
	runhaskell Serve.hs

.PHONY: serve

