package net.sf.royal.datamodel;

public class Bibliotheque
{
	private long id;
	private String name;
	private String address;
	private String phone;

	@Override 
	public String toString() {
		return this.name;
	}
	
	public Bibliotheque() {
	}
	
	public long getId() {
		return this.id;
	}

	private void setId(long nv) {
		this.id = nv;
	}

	public String getName() {
		return this.name;
	}

	public void setName(String nv_name) {
		this.name = nv_name;
	}

	public String getAddress() {
		return this.address;
	}

	public void setAddress(String nv_address) {
		this.address = nv_address;
	}

	public String getPhone() {
		return this.phone;
	}

	public void setPhone(String nv_phone) {
		this.phone = nv_phone;
	}
}
