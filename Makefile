view.js: Stackulator.idr Parser.idr Model.idr HtmlView.idr
	idris -p effects -p lightyear -p iquery -p contrib --codegen javascript HtmlView.idr -o view.js +RTS -M4G

serve: view.js
	@echo "listening."
	runhaskell Serve.hs

.PHONY: serve

