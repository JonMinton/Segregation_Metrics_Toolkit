# Tcl/tk practice session

tt <- tktoplevel()
OK.but <- tkbutton(tt, text="OK", command=function() tkdestroy(tt))

tkgrid(OK.but)

tkfocus(tt)



require(tcktk)
tt <- tktoplevel()
topMenu <- tkmenu(tt)
tkconfigure(tt, menu=topMenu)
fileMenu <- tkmenu(topMenu, tearoff=F)
tkadd(fileMenu, "command", label="Quit", command=function() tkdestroy(tt))
tkadd(topMenu, "cascade", label="File", menu=fileMenu)
tkfocus(tt)


tt <- tktoplevel()
topMenu <- tkmenu(tt)
tkconfigure(tt, menu=topMenu)
fileMenu <- tkmenu(topMenu, tearoff=F)
openRecentMenu <- tkmenu(topMenu, tearoff=F)
tkadd(
    openRecentMenu, 
    "command", 
    label= "Recent File 1", 
    command = function() tkmessageBox(message="I don't know how to open recent file 1", icon="error")
)
tkadd(
    openRecentMenu,
    "command", 
    label="Recent File 2",
    command=function() tkmessageBox(message="I don't know how to open recent file 2", icon="error")
)
tkadd(fileMenu, "cascade", label="open recent file", menu=openRecentMenu)
tkadd(fileMenu, "command", label="Quit", command=function() tkdestroy(tt))
tkadd(topMenu, "cascade", label="File", menu=fileMenu)
tkfocus(tt)


tt <- tktoplevel()
txt <- tktext(tt)
tkgrid(txt)

copyText <- function() .Tcl(paste("event", "generate", .Tcl.args(.Tk.ID(txt), "<<Copy>>")))
editPopupMenu <- tkmenu(txt, tearoff=F)
tkadd(editPopupMenu, "command", label="Copy <Ctrl-C>", command=copyText)

RightClick <- function(x, y){
    rootx <- as.integer(tkwinfo("rootx", txt))
    rooty <- as.integer(tkwinfo("rooty", txt))
    
    xTxt <- as.integer(x) + rootx
    yTxt <- as.integer(y) + rooty
    
    .Tcl(paste("tk_popup", .Tcl.args(editPopupMenu, xTxt, yTxt)))
}

tkbind(txt, "<Button-3>", RightClick)
tkfocus(tt)

require(tcltk)

fileName <- tclvalue(tkgetOpenFile())

if (!nchar(fileName)){
    tkmessageBox(message="No file was selected!")
} else {
    tkmessageBox(message=paste("The file selected was", fileName))
}



modalDialog <- function(
    title,
    question,
    entryInit,
    entryWidth          = 20,
    returnValOnCancel   = "ID_CANCEL"
){
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, title)
    textEntryVarTcl <- tclVar(paste(entryInit))
    textEntryWidget <- tkentry(dlg, width = paste(entryWidth), textvariable=textEntryVarTcl)
    tkgrid(tklabel(dlg, text = "      "))
    tkgrid(tklabel(dlg, text=question), textEntryWidget)
    tkgrid(tklabel(dlg, text= "       "))
    returnVal = returnValOnCancel
    
    onOK <- function(){
        ReturnVal <<- tclvalue(textEntryVarTcl)
        tkgrab.release(dlg)
        tkdestroy(dlg)
        tkfocus(ttMain)
    }
    
    onCancel <- function(){
        ReturnVal <<- returnValOnCancel
        tkgrab.release(dlg)
        tkdestroy(dlg)
        tkfocus(ttMain)
    }
    
    OK.but <- tkbutton(dlg, text= "    OK    ", command=onOK)
    
    Cancel.but <- tkbutton(dlg, text= " Cancel ", command=onCancel)
    tkgrid(OK.but, Cancel.but)
    tkgrid(tklabel(dlg, text = "     "))
    
    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(ttMain)})
    tkbind(textEntryWidget, "<Return>", onOK)
    tkwait.window(dlg)
    
    return(ReturnVal)
}

require(tcltk)

ttMain <- tktoplevel()

tktitle(ttMain) <- "ttMain"

launchDialog <- function() {
    ReturnVal <- modalDialog("First Name Entry", "Enter your First Name", "")
    if (ReturnVal=="ID_CANCEL") return()
    tkmessageBox(title = "greeting", 
                 message = paste("Hello, ", ReturnVal, ".", sep=""))
    
}

launchDlg.button <- tkbutton(ttMain, text="Launch Dialog", command= launchDialog)
tkpack(launchDlg.button)
tkdestroy(ttMain)



require(tcltk)
tt <- tktoplevel()

tktitle(tt) <- "Simple Dialogue"

done <- tclVar(0)


OK.but <- tkbutton(tt, text= "   OK    ",
                   command = function() tclvalue(done) <- 1)
Cancel.but <- tkbutton(tt, text = "Cancel",
                       command = function() tclvalue(done) <- 2)

tkgrid(OK.but, Cancel.but)

tkbind(tt, "<Destroy>", function() tclvalue(done) <- 2)

tkfocus(tt)

tkwait.variable(done)

require(tcltk)

tt <- tktoplevel()
cb <- tkcheckbutton(tt)
cbValue <- tclVar("0")
tkconfigure(cb, variable=cbValue)
tkgrid(tklabel(tt, text= "I like R tclTK "), cb)

OnOK <- function()
{
    cbVal <- as.character(tclvalue(cbValue))
    tkdestroy(tt)
    if(cbVal=="1") tkmessageBox(message="So do I!")
    if(cbVal=="0") tkmessageBox(message="You forgot to check the box", icon="warning")
    
}

OK.but <- tkbutton(tt, text="OK", command = OnOK)

tkgrid(OK.but)
tkfocus(tt)


require(tcltk)

tt <- tktoplevel()
rb1 <- tkradiobutton(tt)
rb2 <- tkradiobutton(tt)
rb3 <- tkradiobutton(tt)

rbValue <- tclVar("oranges")

tkconfigure(rb1, variable=rbValue, value="apples")
tkconfigure(rb2, variable=rbValue, value="oranges")
tkconfigure(rb3, variable=rbValue, value="nada")

tkgrid(tklabel(tt, text="Which do you prefer?"))
tkgrid(tklabel(tt, text="Apples "), rb1)
tkgrid(tklabel(tt, text="Oranges "), rb2)
tkgrid(tklabel(tt, text="Nada "), rb3)

OnOK <- function()
{
    rbVal <- as.character(tclvalue(rbValue))
    tkdestroy(tt)
    
    if (rbVal=="apples") tkmessageBox(message="Good choice")
    if (rbVal=="oranges") tkmessageBox(message="Another good choice")
    if (rbVal=="nada") tkmessageBox(message="You'll be hungry", icon="warning")
}

OK.but <- tkbutton(tt, text="OK", command=OnOK)
tkgrid(OK.but)
tkfocus(tt)


tt <- tktoplevel()
Name <- tclVar("Anonymous")
entry.Name <- tkentry(tt, width="20", textvariable=Name)
tkgrid(tklabel(tt, text="Please enter your first name."))
tkgrid(entry.Name)

OnOK <- function()
{
    NameVal <- tclvalue(Name)
    tkdestroy(tt)
    msg <- paste("You have a nice name,", NameVal)
    tkmessageBox(message=msg)
}

OK.but <- tkbutton(tt, text="     OK     ", command=OnOK)
tkbind(entry.Name, "<Return>", OnOK)
tkgrid(OK.but)
tkfocus(tt)

require(tcltk)
tt <- tktoplevel()
tl <- tklistbox(tt, height=4, selectmode="single", background="white")
tkgrid(tklabel(tt, text="What's your favourite fruit?"))
tkgrid(tl)
fruits <- c("Apple", "Orange", "Banana", "Pear")

for (i in 1:4){
    tkinsert(tl, "end", fruits[i])
}

tkselection.set(tl, 2)

OnOK <- function()
{
    fruitChoice <- fruits[as.numeric(tkcurselection(tl))+1]
    tkdestroy(tt)
    msg <- paste("Good choice! ", fruitChoice, "s are delicious!", sep="")
    
    tkmessageBox(message=msg)
}

OK.but <- tkbutton(tt, text= "    OK    ", command=OnOK)
tkgrid(OK.but)
tkfocus(tt)



require(tcltk)
tt <- tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5, command = function() tkyview(t1, ...))
tl <- tklistbox(tt, height=4, selectmode="single", yscrollcommand=function(...) tkset(scr, ...) , background="white")

tkgrid(tklabel(tt, text="What's your favourite fruit?"))
tkgrid(tl, scr)
tkgrid.configure(scr, rowspan=4, sticky="nsw")
fruits <- c("Apply", "Orange", "Banana", "Pear", "Cherry", "Apricot", "Peach")

for (i in 1:7)
{
    tkinsert(tl, "end", fruits[i])
}

tkselection.set(tl,2)

OnOK <- function()
{
    fruitChoice <- fruits[as.numeric(tkcurselection(tl))+1]
    tkdestroy(tt)
    msg <- paste("Good choice! ", fruitChoice, "s are delicious!", sep="")
    tkmessageBox(message=msg)
}


OK.but <- tkbutton(tt, text="    OK     ", command=OnOK)
tkgrid(OK.but)
tkfocus(tt)




# text windows in R tcltk

tt <- tktoplevel()
txt <- tktext(tt)
tkgrid(txt)
tkmark.set(txt, "insert", "0.0")
tkfocus(txt)


require(tcltk)
tt <- tktoplevel()
txt <- tktext(tt, bg="white", font="courier")
tkgrid(txt)
tkconfigure(txt, state="disabled")
tkinsert(txt, "end", "Hello World")
tkfocus(txt)

require(tcltk)

# Frames
require(tcltk)
tt <- tktoplevel()
frameOverall <- tkframe(tt)
frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=2)
tkgrid(tklabel(frameUpper, text="Text in upper frame"))

frameLower <- tkframe(frameOverall, relief="groove", borderwidth=2)
tkgrid(tklabel(frameLower, text="Text in the lower frame"))

tkgrid(frameUpper)
tkgrid(tklabel(frameOverall, text="Text between the upper and lower frames"))
tkgrid(frameLower)
tkgrid(frameOverall)


# sliders 
require(tcltk)
tt <- tktoplevel()
SliderValue <- tclVar("50")
SliderValueLabel <- tklabel(tt, text=as.character(tclvalue(SliderValue)))
tkgrid(tklabel(tt, text="Slider Value: "), SliderValueLabel, tklabel(tt, text="%"))
tkconfigure(SliderValueLabel, textvariable=SliderValue)
slider <- tkscale(tt, from=100, to=0, showvalue=F, variable=SliderValue, resolution=1, orient="vertical")
tkgrid(slider)
tkfocus(tt)




require(tcltk)

tt <- tktoplevel()
tkwm.title(tt, "Colour selection")
colour <- "blue"

canvas <- tkcanvas(tt, width="80", height="25", bg=colour)
ChangeColour <- function()
{
    colour <- tclvalue(.Tcl(paste("tk_chooseColor", .Tcl.args(initialcolor=colour, title="Choose a colour"))))
    if (nchar(colour)> 0) tkconfigure(canvas, bg=colour)
}
ChangeColour.button <- tkbutton(tt, text="Change colour", command=ChangeColour)
tkgrid(canvas, ChangeColour.button)


# Tree widget
#http://www.sciviews.org/_rgui/tcltk/
#http://www.sciviews.org/_rgui/tcltk/TreeWidget.html
require(tcltk)
tclRequire("BWidget")

tt <- tktoplevel()
tkwm.title(tt, "Tree (Drill-Down) Widget")

xScr <- tkscrollbar(tt, command=function(...) tkxview(treeWidget, ...), orient="horizontal")
yScr <- tkscrollbar(tt, command=function(...) tkyview(treeWidget, ...))
treeWidget <- tkwidget(tt, "Tree", xscrollcommand=function(...) tkset(xScr, ...),
                       yscrollcommand=function(...) tkset(yScr, ...), width=30, height=15)

tkgrid(treeWidget, yScr)
tkgrid.configure(yScr, stick="nsw")
tkgrid(xScr)

tkinsert(treeWidget,"end","root","Record1Node",text="Record 1")
tkinsert(treeWidget,"end","root","Record2Node",text="Record 2")
tkinsert(treeWidget,"end","root","Record3Node",text="Record 3")
tkinsert(treeWidget,"end","root","Record4Node",text="Record 4")

tkinsert(treeWidget,"end","Record1Node","Name1Node",text="Name")
tkinsert(treeWidget,"end","Record2Node","Name2Node",text="Name")
tkinsert(treeWidget,"end","Record3Node","Name3Node",text="Name")
tkinsert(treeWidget,"end","Record4Node","Name4Node",text="Name")

tkinsert(treeWidget,"end","Record1Node","Age1Node",text="Age")
tkinsert(treeWidget,"end","Record2Node","Age2Node",text="Age")
tkinsert(treeWidget,"end","Record3Node","Age3Node",text="Age")
tkinsert(treeWidget,"end","Record4Node","Age4Node",text="Age")

tkinsert(treeWidget,"end","Name1Node","Name1Val",text="Fred")
tkinsert(treeWidget,"end","Name2Node","Name2Val",text="Jane")
tkinsert(treeWidget,"end","Name3Node","Name3Val",text="Tim")
tkinsert(treeWidget,"end","Name4Node","Name4Val",text="Alex")

tkinsert(treeWidget,"end","Age1Node","Age1Val",text="14")
tkinsert(treeWidget,"end","Age2Node","Age2Val",text="35")
tkinsert(treeWidget,"end","Age3Node","Age3Val",text="63")
tkinsert(treeWidget,"end","Age4Node","Age4Val",text="52")