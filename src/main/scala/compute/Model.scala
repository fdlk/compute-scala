package compute

object model {
  type Parameter = String
  type Value = String
  type Template = String
  type ParameterCombination = Map[Parameter, Value]
  case class Task(val step: Step, val index: Int, val inputValues: List[Value], val inputList: Iterable[ParameterCombination]) {
    def id: String = step.name + '_' + index
    override def toString = "Task[" + id + ", " + inputValues.toString + ", " + inputList.map(_.values.toList).toString + ']'
  }
  case class Step(val name: String,
                  val protocol: Template,
                  val singleInputs: List[Parameter] = Nil,
                  val listInputs: List[Parameter] = Nil,
                  val outputs: List[Parameter] = Nil) {
    def inputs = singleInputs ::: listInputs
    def filterInputs(parameters: Iterable[ParameterCombination]) = parameters.map(pc => pc.filterKeys { inputs.contains(_) })
    def filterListInputs(parameters: Iterable[ParameterCombination]) = parameters.map(pc => pc.filterKeys { listInputs.contains(_) })
    def tasks(inputValues: Iterable[ParameterCombination]): Iterable[Task] = {
      inputValues
        .groupBy(combination => singleInputs.flatMap(combination.get))
        .zipWithIndex
        .map({ case ((singleInputValues, pc), index) => Task(this, index, singleInputValues, filterListInputs(pc)) })
    }
  }
  case class Workflow(val name: String, val steps: List[Step]) {
    def filterInputsForStep(step: Step, parameters: List[ParameterCombination]): List[ParameterCombination] = {
      parameters.map(pc => pc.filterKeys { step.inputs.contains(_) })
    }

    def tasks(parameters: List[Map[Parameter, Value]]): Iterable[Task] = {
      val step = steps.head
      step.tasks(filterInputsForStep(step, parameters))
    }
  }
}
