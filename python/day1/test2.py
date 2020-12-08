#!/usr/bin/env python
import io
from part2 import solution

text = io.StringIO("""1721
979
366
299
675
1456""")


assert solution(text) == 241861950