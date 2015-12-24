object Config {
  // BoardSize command of GTP only can change this value
  var dia = 19
  def all = dia * dia
  def padDia = dia + 2
  def padAll = (dia + 2) * (dia + 2)
  var Komi = 5.5
}
