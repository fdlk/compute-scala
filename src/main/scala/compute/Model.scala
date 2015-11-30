package compute

object model {
  type Parameter = String
  type Value = String
  type Template = String
  type ParameterCombination = Map[Parameter, Value]
  type TaskGeneratorState = (List[Task], List[ParameterCombination])
  // extra parameter to introduce to keep track of the parameter combinations
  val PC_ID = "pcID"

  case class Task(stepName: String, template: Template, index: Int, inputValues: ParameterCombination,
                  inputList: List[ParameterCombination]) {
    def id: String = stepName + '_' + index

    override def toString = "Task[" + id + ", " + inputValues + ", " + inputList + "]\n"
  }

  case class Protocol(template: Template,
                      singleInputs: List[Parameter] = Nil,
                      listInputs: List[Parameter] = Nil,
                      outputs: List[Parameter] = Nil) {
    def filterListInputs(groupedInputValues:List[ParameterCombination]): List[ParameterCombination] =
      groupedInputValues.map(_.filterKeys((PC_ID :: listInputs).contains))
    def tasks(stepName: String, inputValues: List[ParameterCombination]): List[Task] = {
      inputValues
        .groupBy(combination => singleInputs.flatMap(combination.get))
        .zipWithIndex
        .map({ case ((singleInputValues, groupedInputValues), index) =>
          Task(stepName, template, index, singleInputs.zip(singleInputValues).toMap, filterListInputs(groupedInputValues))
        }).toList
    }
  }

  case class Step(name: String, protocol: Protocol, localToGlobal: Map[Parameter, Parameter] = Map()) {
    def globalToLocalMap = localToGlobal.map(_.swap)

    def globalToLocal(parameters: ParameterCombination): ParameterCombination =
      parameters.map({ case (globalName, value) => (globalToLocalMap.get(globalName).getOrElse(globalName), value) })

    def globalToLocal(parameters: List[ParameterCombination]): List[ParameterCombination] =
      parameters.map(globalToLocal)
        .zipWithIndex
        .map({ case (pc, id) => pc.updated(PC_ID, id.toString) })

    def tasks(global: List[ParameterCombination]): Iterable[Task] = {
      protocol.tasks(name, globalToLocal(global))
    }
  }

  case class Workflow(name: String, steps: List[Step]) {
    def addTaskOutcomeToParameterCombinations(outputParameter: Parameter, tasks: List[Task], pcs: List[ParameterCombination]): List[ParameterCombination] = {
      val taskIds = tasks.flatMap(task => task.inputList.map(pc => (pc.get(PC_ID).get, task.id))).toMap
      pcs.zipWithIndex.map({ case (pc, id) => pc.updated(outputParameter, taskIds.get(id.toString).get) })
    }

    def reduce(state: TaskGeneratorState, step: Step): TaskGeneratorState = state match {
      case (tasks, parameters) => {
        val stepTasks = step.tasks(parameters).toList
        (tasks ++ stepTasks, addTaskOutcomeToParameterCombinations(step.name + ".out", stepTasks, parameters))
      }
    }

    def tasks(parameters: List[ParameterCombination]): List[Task] = steps.foldLeft((Nil: List[Task], parameters))(reduce)._1
  }

}
