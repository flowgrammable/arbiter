// Copyright (C) 2013, Texas A&M University.
// All rights reserved.
// Written by Gabriel Dos Reis.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     - Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     - Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//
//     - Neither the name of Liz, nor the names of its contributors may
//       be used to endorse or promote products derived from this software
//       without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
// OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <QMenuBar>
#include <QStatusBar>
#include "MainWindow.H"

namespace liz {
   // Add `New' action to the `File' menu.
   static void
   add_new_action(ide::MainWindow* win, QMenu* menu) {
      auto action = new QAction(QObject::tr("&New"), win);
      action->setShortcut(QObject::tr("Ctrl+N"));
      // FIXME: Connect.
      menu->addAction(action);
   }
   
   // Add `Open' action to the `File' menu.
   static void
   add_open_action(ide::MainWindow* win, QMenu* menu) {
      auto action = new QAction(QObject::tr("&Open"), win);
      action->setShortcut(QObject::tr("Ctrl+O"));
      // FIXME: Connect.
      menu->addAction(action);
   }
   
   // Add `Save' action to the `File' menu.
   static void
   add_save_action(ide::MainWindow* win, QMenu* menu) {
      auto action = new QAction(QObject::tr("&Save"), win);
      action->setShortcut(QObject::tr("Ctrl+S"));
      // FIXME: Connect.
      menu->addAction(action);
   }
   
   // Add `Exit' action to the `File' menu.
   static void
   add_exit_action(ide::MainWindow* win, QMenu* menu) {
      auto action = new QAction(QObject::tr("E&xit"), win);
      action->setShortcut(QObject::tr("Ctrl+Q"));
      QObject::connect(action, SIGNAL(triggered()), win, SLOT(close()));
      menu->addAction(action);
   }

   static void
   populate_file_menu(ide::MainWindow* win, QMenu* menu) {
      add_new_action(win, menu);
      add_open_action(win, menu);
      add_save_action(win, menu);
      menu->addSeparator();
      add_exit_action(win, menu);
   }

   static void
   populate_edit_menu(ide::MainWindow* win) {
      auto menu = win->editor()->createStandardContextMenu();
      menu->setTitle(QObject::tr("&Edit"));
      win->menuBar()->addMenu(menu);
   }

   static void
   populate_options_menu(ide::MainWindow*, QMenu*) {
   }

   static void
   populate_tools_menu(ide::MainWindow*, QMenu*) {
   }

   static void
   populate_help_menu(ide::MainWindow*, QMenu*) {
   }
   
   ide::MainWindow::MainWindow() : box(this), ed(this) {
      setCentralWidget(&box);
      box.addWidget(&ed);
      box.addWidget(&out);
      populate_file_menu(this, menuBar()->addMenu(tr("&File")));
      populate_edit_menu(this);
      menuBar()->addSeparator();
      populate_options_menu(this, menuBar()->addMenu(tr("&Options")));
      populate_tools_menu(this, menuBar()->addMenu(tr("&Tools")));
      populate_help_menu(this, menuBar()->addMenu(tr("&Help")));
      editor()->setContextMenuPolicy(Qt::ActionsContextMenu);
      statusBar()->showMessage(tr("Ready for editing"));

      out.setReadOnly(true);

      connect(editor()->document(), SIGNAL(contentsChange(int, int, int)),
              outlet(), SLOT(act_on_edits(int, int, int)));

      connect(editor(), SIGNAL(cursorPositionChanged()),
              this, SLOT(display_line_column()));
   }

   ide::MainWindow::~MainWindow() {
   }

   void
   ide::MainWindow::display_line_column() {
      auto cur = editor()->textCursor();
      auto l = QString::number(1 + cur.blockNumber());
      auto c = QString::number(cur.columnNumber());
      statusBar()->showMessage("(" + l + "," + c + ")");
   }
}
