import Effects exposing (Never)
import NBack
import StartApp
import Task

app =
  StartApp.start
    { init = NBack.init
    , update = NBack.update
    , view = NBack.view
    , inputs = []
    }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
