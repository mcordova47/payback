'use strict'

const Elm = require('./Main.elm')
const mountNode = document.getElementById('main')

const app = Elm.Main.embed(mountNode)

app.ports.upload.subscribe(file => {
  const fileReader = new FileReader()
  fileReader.onload = () => app.ports.readFile.send(fileReader.result)
  fileReader.readAsText(file)
})
