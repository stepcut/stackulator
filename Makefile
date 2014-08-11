view.js: Stackulator.idr Model.idr HtmlView.idr
	idris -p effects -p lightyear -p iquery --codegen javascript HtmlView.idr -o view.js
