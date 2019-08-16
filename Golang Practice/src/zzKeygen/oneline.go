/*
 * Created by Steven Jennings (zzApotheosis) on 15 August, 2019.
 */

package main

import (
  _ "fmt"
  "strconv"
  "jenningsUtil"
  "os"
)

// oneLineExecute This does not copy to system clipboard because
// it is designed to be used in shell scripting on Linux.
func oneLineExecute(args []string) {
  // Define variables
  var fullseed, charset string
	var length, shift int
  var err error

  // Parse arguments
  fullseed = args[0] + args[1]
  length, err = strconv.Atoi(args[2]); util.Check(err)
  shift, err = strconv.Atoi(args[3]); util.Check(err)
  charset = args[4]

  // Process data, print to stdout for use in Linux piping. God, Linus Torvalds is big daddy.
  util.SetSeedAsString(fullseed)
	os.Stdout.Write([]byte(util.RandomString(length, charset, shift)))
}
