default: build

build:
  @echo "Compiling fennel files"
  @fd -t=f -e=task.fnl -x sh -c "fennel --compile {} > {.}.lua"

clean:
  @echo "Removing compiled fennel files"
  @fd -t=f -e=task.lua -X echo removing {}
  @fd -t=f -e=task.lua -x rm -f {}
