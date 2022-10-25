package markup;

import java.util.*;

class Text implements Markable{
	String s;
	public Text(String listok)
	{
		this.s = listok;
	}
 
	@Override
	public void toMarkdown(StringBuilder str)
	{
		str.append(this.s);
	}
	
	@Override
	public void toBBCode(StringBuilder str)
	{
		str.append(this.s);
	}
}