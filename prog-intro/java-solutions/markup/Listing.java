package markup;

import java.util.*;

public abstract class Listing implements Listable {

	public static void abstractBBCode(List<ListItem> s, StringBuilder str, String markBefore, String markAfter)
	{
		str.append(markBefore);
		for(ListItem pi : s)
			pi.toBBCode(str);
		str.append(markAfter);
	}
}