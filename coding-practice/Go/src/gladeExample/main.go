/*
 * Created by Steven Jennings (zzApotheosis) on 12 April 2020.
 *
 * Template started from: https://gist.github.com/Northern-Lights/14f2357aebb8ccbf9b351049035c151d
 */

package main

import (
	"fmt"
	"log"
	"os"
	"reflect"

	"github.com/gotk3/gotk3/glib"

	"github.com/gotk3/gotk3/gtk"
)

// Built a small UI using Glade. Save the project, and the .glade file is this XML
const gladeTemplate = `<?xml version="1.0" encoding="UTF-8"?>
 <!-- Generated with glade 3.22.1 -->
 <interface>
   <requires lib="gtk+" version="3.20"/>
   <object class="GtkWindow" id="window">
     <property name="can_focus">False</property>
     <child>
       <placeholder/>
     </child>
     <child>
       <object class="GtkBox">
         <property name="visible">True</property>
         <property name="can_focus">False</property>
         <property name="orientation">vertical</property>
         <child>
           <object class="GtkButton" id="b1">
             <property name="label" translatable="yes">button 1</property>
             <property name="visible">True</property>
             <property name="can_focus">True</property>
             <property name="receives_default">True</property>
             <signal name="clicked" handler="B1" swapped="no"/>
           </object>
           <packing>
             <property name="expand">False</property>
             <property name="fill">True</property>
             <property name="position">0</property>
           </packing>
         </child>
         <child>
           <object class="GtkButton" id="b2">
             <property name="label" translatable="yes">button 2</property>
             <property name="visible">True</property>
             <property name="can_focus">True</property>
             <property name="receives_default">True</property>
             <signal name="clicked" handler="B2" swapped="no"/>
           </object>
           <packing>
             <property name="expand">False</property>
             <property name="fill">True</property>
             <property name="position">1</property>
           </packing>
         </child>
         <child>
           <object class="GtkButton" id="b3">
             <property name="label" translatable="yes">button 3</property>
             <property name="visible">True</property>
             <property name="can_focus">True</property>
             <property name="receives_default">True</property>
             <signal name="clicked" handler="B3" swapped="no"/>
           </object>
           <packing>
             <property name="expand">False</property>
             <property name="fill">True</property>
             <property name="position">2</property>
           </packing>
         </child>
       </object>
     </child>
   </object>
 </interface>
 `

// looks like handlers can literally be any function or method
func b1Clicked() {
	fmt.Println("b1 clicked")
}

func b2Clicked() {
	fmt.Println("b2 clicked")
}

func b3Clicked() {
	fmt.Println("b3 clicked")
}

// you just place them in a map that names the signals, then feed the map to the builder
var signals = map[string]interface{}{
	"B1": b1Clicked,
	"B2": b2Clicked,
	"B3": b3Clicked,
}

func main() {
	const appID = "com.retc3.mytest"
	app, err := gtk.ApplicationNew(appID, glib.APPLICATION_FLAGS_NONE)
	if err != nil {
		log.Fatalln("Couldn't create app:", err)
	}

	// It looks like all builder code must execute in the context of `app`.
	// If you try creating the builder inside the main function instead of
	// the `app` "activate" callback, then you will get a segfault
	app.Connect("activate", func() {
		// Use this instead if you have your glade XML in a separate file
		// builder, err := gtk.BuilderNewFromFile("mytest.glade")
		builder, err := gtk.BuilderNew()
		if err != nil {
			log.Fatalln("Couldn't make builder:", err)
		}

		// If you used gtk.BuilderNewFromFile, you won't need this
		err = builder.AddFromString(gladeTemplate)
		if err != nil {
			log.Fatalln("Couldn't add UI XML to builder:", err)
		}

		builder.ConnectSignals(signals)

		obj, err := builder.GetObject("b1")
		if err != nil {
			log.Fatalln("Couldn't get button")
		}
		b1, ok := obj.(*gtk.Button)
		if !ok {
			log.Fatalln("It wasn't a button; it was a", reflect.TypeOf(obj))
		}
		log.Println("The button is", b1)

		obj, err = builder.GetObject("window")
		wnd := obj.(*gtk.Window)
		wnd.ShowAll()
		app.AddWindow(wnd)
	})
	app.Run(os.Args)
}
