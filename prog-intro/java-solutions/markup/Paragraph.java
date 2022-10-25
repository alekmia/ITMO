package markup;

import java.util.*;

public class Paragraph extends Listing {
	List<Markable> s;
	public Paragraph(List<Markable> listok)
	{
		this.s = listok;
	}

	public void toMarkdown(StringBuilder str)
	{
		for(Markable pi : s)
			pi.toMarkdown(str);
	}

	@Override
	public void toBBCode(StringBuilder str) {
		for(Markable pi : s)
			pi.toBBCode(str);
	}
}

