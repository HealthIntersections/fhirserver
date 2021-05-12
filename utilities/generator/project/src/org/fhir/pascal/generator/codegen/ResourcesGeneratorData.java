package org.fhir.pascal.generator.codegen;

public class ResourcesGeneratorData {
	public StringBuilder rt;
	public StringBuilder rf;
	public StringBuilder ra1;
	public StringBuilder rc1;
	public StringBuilder ra2;
	public StringBuilder rc2;
	public StringBuilder redeclare = new StringBuilder();

	public ResourcesGeneratorData(StringBuilder rt, StringBuilder rf, StringBuilder ra1, StringBuilder rc1,
			StringBuilder ra2, StringBuilder rc2) {
		this.rt = rt;
		this.rf = rf;
		this.ra1 = ra1;
		this.rc1 = rc1;
		this.ra2 = ra2;
		this.rc2 = rc2;
	}
}