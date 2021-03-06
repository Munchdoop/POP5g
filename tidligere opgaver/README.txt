Oversæt og kør koden
- Åben din Kommando-linje eller terminal
- Bevæg dig ind i den sti hvori filerne ligger
- Skriv "fsharpc 5g0.fsx" i terminalen
- Skriv "fsharpc 5g1.fsx" i terminalen
- Skriv "mono 5g0.exe" i terminalen
- Skriv "mono 5g1.exe" i terminalen
Koden kører nu

White-box "isTable"
+---------+--------+----------------------------------------+-----------------+-----------------+---------------------------+
| Unit    | Branch | Condition                              | Input           | Expected output | Comment                   |
+---------+--------+----------------------------------------+-----------------+-----------------+---------------------------+
| isTable | 1      | lenFirstColumn = 0                     |                 |                 |                           |
+---------+--------+----------------------------------------+-----------------+-----------------+---------------------------+
|         | 1a     | t                                      | [[]]            | false           | Failed because empty      |
+---------+--------+----------------------------------------+-----------------+-----------------+---------------------------+
|         | 1b     | f                                      | [[1;2];[1;2;2]] |                 | move to next branch       |
+---------+--------+----------------------------------------+-----------------+-----------------+---------------------------+
|         | -      | i < lenRows && isValidTable            |                 |                 |                           |
+---------+--------+----------------------------------------+-----------------+-----------------+---------------------------+
|         | 2      | llstCopy.Head.Length <> lenFirstColumn |                 |                 |                           |
+---------+--------+----------------------------------------+-----------------+-----------------+---------------------------+
|         | 2a     | t                                      | [[1;2];[1;2;2]] | false           | Failed because inequality |
+---------+--------+----------------------------------------+-----------------+-----------------+---------------------------+
|         | 2b     | f                                      | [[1;2];[1;2]]   | true            | Ended with true           |
+---------+--------+----------------------------------------+-----------------+-----------------+---------------------------+

White-box "firstColumn"
+-------------+--------+-------------------------------------+---------------+-----------------+---------+
| Unit        | Branch | Condition                           | Input         | Expected output | Comment |
+-------------+--------+-------------------------------------+---------------+-----------------+---------+
| firstColumn | 1      | List.contains (true) (llstHasEmpty) |               |                 |         |
+-------------+--------+-------------------------------------+---------------+-----------------+---------+
|             | 1a     | t                                   | [[]]          | []              |         |
+-------------+--------+-------------------------------------+---------------+-----------------+---------+
|             | 1b     | f                                   | [[1;2];[1;2]] | [1;1]           |         |
+-------------+--------+-------------------------------------+---------------+-----------------+---------+

White-box "dropFirstColumn"
+-----------------+--------+-------------------------------------+---------------+-----------------+---------+
| Unit            | Branch | Condition                           | Input         | Expected output | Comment |
+-----------------+--------+-------------------------------------+---------------+-----------------+---------+
| dropFirstColumn | 1      | List.contains (true) (llstHasEmpty) |               |                 |         |
+-----------------+--------+-------------------------------------+---------------+-----------------+---------+
|                 | 1a     | t                                   | [[]]          | [[]]            |         |
+-----------------+--------+-------------------------------------+---------------+-----------------+---------+
|                 | 1b     | f                                   | [[1;2];[1;2]] | [[2];[2]]       |         |
+-----------------+--------+-------------------------------------+---------------+-----------------+---------+

White-box "transposeLstLst"
+-----------------+--------+-------------------------------------------------+-----------------+-----------------+
| Unit            | Branch | Condition                                       | Input           | Expected output |
+-----------------+--------+-------------------------------------------------+-----------------+-----------------+
| transposeLstLst | 1      | List.forall (fun (x:'a list) -> x.Length > 0) a |                 |                 |
+-----------------+--------+-------------------------------------------------+-----------------+-----------------+
|                 | 1a     | t                                               | [[]]            | [[]]            |
+-----------------+--------+-------------------------------------------------+-----------------+-----------------+
|                 | 1b     | f                                               | [[1;2];[1;2]]   | [[1;1];[2;2]]   | 
+-----------------+--------+-------------------------------------------------+-----------------+-----------------+

White-box "transposeArr"
+--------------+--------+-----------+---------+------------------+--------------------------------+
| unit         | Branch | Condition | Input   | Expected output  | Comment                        |
+--------------+--------+-----------+---------+------------------+--------------------------------+
| TransposeArr | 1      |     -     | array2d |                  |                                |
+--------------+--------+-----------+---------+------------------+--------------------------------+
| newArray     | 2      |     -     | array2d | transposed array | No conditions in this function |
+--------------+--------+-----------+---------+------------------+--------------------------------+
