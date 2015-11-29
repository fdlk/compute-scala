package compute

object model {
  type Parameter = String
  type Value = String
  type Template = String
  type ParameterCombination = Map[Parameter, Value]
  val PC_ID: Parameter = "pcID"

  // extra parameter to introduce to keep track of the parameter combinations

  case class Task(stepName: String, template: Template, index: Int, inputValues: ParameterCombination,
                  inputList: List[ParameterCombination]) {
    def id: String = stepName + '_' + index
    override def toString = "Task[" + id + ", " + inputValues + ", " + inputList + "]\n"
  }

  case class Protocol(template: Template,
                      singleInputs: List[Parameter] = Nil,
                      listInputs: List[Parameter] = Nil,
                      outputs: List[Parameter] = Nil) {
    def tasks(stepName: String, inputValues: List[ParameterCombination]): List[Task] = {
      inputValues
        .groupBy(combination => singleInputs.flatMap(combination.get))
        .zipWithIndex
        .map({ case ((singleInputValues, listInputValues), index) =>
          Task(stepName, template, index, singleInputs.zip(singleInputValues).toMap,
            listInputValues.map(liv => liv.filterKeys(k => (PC_ID :: listInputs).contains(k))))
        }).toList
    }
  }

  case class Step(name: String, protocol: Protocol, localToGlobal: Map[Parameter, Parameter]) {
    def globalToLocal(globalName: String) = localToGlobal.find(_._2 == globalName).map(_._1).getOrElse(globalName)

    def globalToLocal(parameters: ParameterCombination): ParameterCombination =
      parameters.map({ case (globalName, value) => (globalToLocal(globalName), value) })

    def globalToLocal(parameters: List[ParameterCombination]): List[ParameterCombination] =
      parameters.map(globalToLocal)
        .zipWithIndex
        .map({ case (pc, i) => pc.updated(PC_ID, "" + i) })

    def tasks(global: List[ParameterCombination]): Iterable[Task] = {
      protocol.tasks(name, globalToLocal(global))
    }
  }

  case class Workflow(name: String, steps: List[Step]) {
    def tasks(parameters: List[ParameterCombination]) = {
      //TODO: Make this a foldLeft on the steps or so
      val step = steps.head
      val tasks = step.tasks(parameters)
      val taskIds = tasks.flatMap(task => task.inputList.map(pc => (pc.get(PC_ID).get, task.id))).toMap
      val parameters2 = parameters.zipWithIndex.map({
        case (pc, i) => pc.updated(step.name+".out", taskIds.get(""+i).get)
      })
      val step2 = steps.tail.head
      val tasks2 = step2.tasks(parameters2)
      val taskIds2 = tasks2.flatMap(task => task.inputList.map(pc => (pc.get(PC_ID).get, task.id))).toMap
      val parameters3 = parameters2.zipWithIndex.map({
        case (pc, i) => pc.updated(step2.name+".out", taskIds2.get(""+i).get)
      })
      println(parameters3)
      tasks ++ tasks2
    }
  }

}
