import Lake
open Lake DSL

package «class-compiler» {
  -- add package configuration options here
}

lean_lib ClassCompiler {
  -- add library configuration options here
}

@[default_target]
lean_exe «class-compiler» {
  root := `Main
}
