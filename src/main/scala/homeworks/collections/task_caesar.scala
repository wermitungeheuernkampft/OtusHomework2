package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */

  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   *         task"Реализуйте метод `encrypt`"()
   */
  def encrypt(word: String, offset: Int): String = {
    def e(a: Char, step: Int = offset, abc: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ") = {
      abc((abc.indexOf(a.toUpper) + step) % abc.length)
    }

    word.map(e(_))
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    def e(a: Char, step: Int = offset, abc: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ") = {
      val result = abc.indexOf(a.toUpper) - step % abc.length
      result < 0 match {
        case true => abc(result + abc.length)
        case false => abc(result)
      }
    }

    cipher.map(e(_))
  }
}
