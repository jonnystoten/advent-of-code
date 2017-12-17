class HighEntropyPassphrases extends Solver {
  def part1(input: String): Int = {
    val passphrases = input.split('\n')
    def isValidPassphrase(passphrase: String): Boolean = {
      val words = passphrase.split(' ')
      words.distinct.length == words.length
    }
    passphrases.count(p => isValidPassphrase(p))
  }

  def part2(input: String): Int = {
    val passphrases = input.split('\n')
    def isValidPassphrase(passphrase: String): Boolean = {
      val words = passphrase.split(' ').map(w => w.sorted)
      words.distinct.deep == words.deep
    }
    passphrases.count(p => isValidPassphrase(p))
  }
}
