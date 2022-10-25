package markup;

import java.util.*;

public abstract class Marking implements Markable{
	public void abstractMark(List<Markable> s, StringBuilder str, String mark)
	{
		str.append(mark);
		for(Markable pi : s)
			pi.toMarkdown(str);
		str.append(mark);
	}
	public void abstractBBCode(List<Markable> s, StringBuilder str, String markBefore, String markAfter)
	{
		str.append(markBefore);
		for(Markable pi : s)
			pi.toBBCode(str);
		str.append(markAfter);
	}
}